package srv

import (
	"bufio"
	"bytes"
	"crypto/sha1"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/gin-gonic/gin"
	"github.com/mgutz/logxi/v1"
	"github.com/toqueteos/webbrowser"
	"gopkg.in/yaml.v2"

	"coligui/wui"
)

type (
	Job struct {
		Name string
		//		Id   string
		JsonSha1 string
		YamlSha1 string
		Root     Node
	}

	Node struct {
		Id          string
		Type        string
		Label       string
		Description string
		Value       interface{}
		CmdLet      string
		Kids        []*Node
		//		IsActive *bool `json:"active"`
	}

	nodesById_M map[string]*Node

	//	cmdletsById_M map[string]string

	errHandler_T struct {
		err error
	}
)

func (eh *errHandler_T) safe(step func()) {
	if eh.err == nil {
		step()
		//		eh.ifErr(func() { log.Printf("ERROR: %s\n", eh.err) })
		eh.ifErr(func() { log.Error("ERROR", "err", eh.err) })
	}
}

func (eh *errHandler_T) ifErr(handle func()) bool {
	if eh.err != nil {
		handle()
	}
	return eh.err != nil
}

func (job *Job) Check() error {
	if strings.TrimSpace(job.Name) == "" {
		return errors.New("MISSING job Name")
	}
	if strings.TrimSpace(job.Root.Label) == "" {
		return errors.New("MISSING job Root Label")
	}
	return job.Root.ProcessTree()
}

func (node *Node) ProcessTree() error {
	nodesById_m := make(nodesById_M)

	cnf := func(n *Node) error {
		return n.CheckNode(nodesById_m)
	}

	return node.WalkTree(cnf)
}

func (node *Node) CheckNode(nodesById_m nodesById_M) error {
	altNode, ok := nodesById_m[node.Id]
	if ok {
		errMsg := fmt.Sprintf("Duplicate ID '%s':  %+v  <->  %+v",
			node.Id, altNode, node)
		return errors.New(errMsg)
	}
	nodesById_m[node.Id] = node
	return nil
}

func (node *Node) WalkTree(cnf func(*Node) error) error {
	for _, kid := range node.Kids {
		err := kid.WalkTree(cnf)
		if err != nil {
			return err
		}
	}

	return cnf(node)
}

//func ServeGin(port int, baseDir string, htmlFiles_l []string) error {
//	//	baseDir := "/tmp"
//	return ServeGinX(port, baseDir, htmlFiles_l)
//}

func ServeGin(port int, baseDir string, htmlFiles_l []string) error {
	router := gin.Default()

	router.GET("/", func(c *gin.Context) {
		eh := errHandler_T{}
		var index_b []byte
		eh.safe(func() {
			for _, fn := range htmlFiles_l {
				index_b, eh.err = ioutil.ReadFile(fn)
				if eh.err == nil {
					var n int64
					n, eh.err = io.Copy(c.Writer, bytes.NewBuffer(index_b))
					log.Info("serving local file index.html", "file", fn, "size", n, "err", eh.err)
					break
				}
			}
			if len(index_b) == 0 {
				eh.err = wui.WriteWuiHtml(c.Writer)
				log.Info("serving builtin index.html", "err", eh.err)
			}
		})

		eh.ifErr(func() { c.AbortWithError(http.StatusBadRequest, eh.err) })
	})

	router.POST("/jobs/:cmd", func(c *gin.Context) {
		time.Sleep(300 * time.Millisecond)
		eh := errHandler_T{}
		eh.handleJobPost(baseDir, c)
	})

	router.GET("/jobs/:cmd", func(c *gin.Context) {
		time.Sleep(300 * time.Millisecond)
		eh := errHandler_T{}
		eh.handleJobList(baseDir, c)
	})

	router.GET("/ping", func(c *gin.Context) {
		c.JSON(200, gin.H{
			"message": "pong",
		})
	})

	port_s := ""
	if port > 0 && port < 65536 {
		port_s = fmt.Sprintf(":%d", port)
	}
	url := fmt.Sprintf("http://localhost%s/", port_s)

	go func() {
		time.Sleep(300 * time.Millisecond)
		err := webbrowser.Open(url)
		if err != nil {
			//			log.Printf("FAILED to open url in browser: %s\n", err)
			log.Error("FAILED to open url in browser", "err", err)
		}
	}()

	return router.Run(port_s) // listen and server on 0.0.0.0:8080
}

func (eh *errHandler_T) handleJobList(baseDir string, c *gin.Context) error {
	//... parse JSON in post body
	defer c.Request.Body.Close()

	cmdName := c.Param("cmd")
	//		id_s := c.Param("id")
	if cmdName != "RSync" {
		panic("UNKNOWN cmdName = " + cmdName)
	}

	//	eh.safe(func() {
	//		res := gin.H{
	//			"job_type": gin.H{
	//				"name": "RSync",
	//				"id":   "x0",
	//				"jobs": []gin.H{
	//					gin.H{
	//						"name":    "hra",
	//						"json_id": "x1",
	//						"yaml_id": "x1",
	//						"cmd":     "do IT!",
	//						"versions": []string{
	//							"v1", "v2", "v3",
	//						},
	//					},
	//					gin.H{
	//						"name":    "kati",
	//						"json_id": "x1",
	//						"yaml_id": "x1",
	//						"cmd":     "do IT!",
	//						"versions": []string{
	//							"v1", "v2", "v3",
	//						},
	//					},
	//					gin.H{
	//						"name":    "default",
	//						"json_id": "x1",
	//						"yaml_id": "x1",
	//						"cmd":     "do IT!",
	//						"versions": []string{
	//							"v1", "v2", "v3",
	//						},
	//					},
	//				},
	//			},
	//		}
	//		c.JSON(http.StatusOK, res)
	//	})

	//	eh.ifErr(func() { c.AbortWithError(http.StatusBadRequest, eh.err) })
	//	return eh.err

	log.Info("loading jobs", "cmd", cmdName)

	var jobs_ml []gin.H

	eh.safe(func() {
		eh.forAllJobs(
			baseDir, cmdName, "*", //-- jobName,
			"", //	"."+cs, // toIgnore,

			// cmdDir, cmdName, jobName
			func(oldJobFPath string, oldJob_b []byte) error {
				log.Info("found job", "cmd", cmdName,
					"jobfile", oldJobFPath, "size", len(oldJob_b))

				var cfg_b []byte
				eh.safe(func() {
					cfg_b, eh.err = extractYamlConfig(oldJob_b)
				})
				log.Info("extracted job config", "cmd", cmdName,
					"jobfile", oldJobFPath, "size", len(cfg_b))

				var job Job
				eh.safe(func() {
					eh.err = yaml.Unmarshal(cfg_b, &job)
				})
				log.Info("parsed job", "cmd", cmdName,
					"jobfile", oldJobFPath, "name", job.Name)

				job_m := gin.H{
					"name":    job.Name,
					"json_id": job.JsonSha1,
					"yaml_id": job.YamlSha1,
					"cmd":     job.Root.CmdLet,
				}

				jobs_ml = append(jobs_ml, job_m)

				eh.ifErr(func() { c.AbortWithError(http.StatusInternalServerError, eh.err) })
				return eh.err
			},
		)
	})

	eh.safe(func() {
		res := gin.H{
			"job_type": gin.H{
				"name": cmdName,
				"id":   "x0",
				"jobs": jobs_ml,
			},
		}
		c.JSON(http.StatusOK, res)
	})

	eh.ifErr(func() { c.AbortWithError(http.StatusBadRequest, eh.err) })
	return eh.err
}

func /*(eh *errHandler_T)*/ extractYamlConfig(job_b []byte) (cfg_b []byte, err error) {
	//	var cfg_b []byte
	jobScanner := bufio.NewScanner(bytes.NewBuffer(job_b))
	isYaml := false
	for jobScanner.Scan() {
		line_s := jobScanner.Text()
		if strings.HasPrefix(line_s, "# begin:  CoLiGui job configuration for:") {
			isYaml = true
		} else if strings.HasPrefix(line_s, "# end:  CoLiGui job configuration for:") {
			isYaml = false
		}
		if isYaml {
			//			log.Info("extractYamlConfig", "line", fmt.Sprintf("%#v", []byte(line_s)))
			cfg_b = append(cfg_b, []byte(line_s+"\n")...)
		}
	}
	err = jobScanner.Err()
	return cfg_b, err
}

func (eh *errHandler_T) handleJobPost(baseDir string, c *gin.Context) error {
	cmd_s := c.Param("cmd")
	//		id_s := c.Param("id")

	//... parse JSON in post body
	defer c.Request.Body.Close()

	var body_b []byte
	eh.safe(func() { body_b, eh.err = ioutil.ReadAll(c.Request.Body) })
	//	msg1 := fmt.Sprintf("POSTed to /job/%s: %d bytes ...", cmd_s, len(body_b))
	//	fmt.Println(msg1)
	log.Info("POSTed to /job", "cmd", cmd_s, "bytes", len(body_b))

	var job Job
	eh.safe(func() { eh.err = json.Unmarshal(body_b, &job) })
	eh.safe(func() {
		eh.err = job.Check()
		//		fmt.Printf("got '%s': '''%s''' from %#v\n", cmd_s, cmdRes, node)
		//			fmt.Printf("got '%s': %#v: %v\n", cmd_s, job, eh.err)
		//		fmt.Printf("got '%s': err=%v\n", cmd_s, eh.err)
	})

	eh.safe(func() {
		//		jsonSha1 := eh.hashSha1(job, json.Marshal)
		//		job.YamlSha1 = eh.hashSha1(job, yaml.Marshal)
		//		job.JsonSha1 = jsonSha1
		job.JsonSha1, job.YamlSha1 =
			eh.hashSha1(job, json.Marshal),
			eh.hashSha1(job, yaml.Marshal)
	})

	var job2_yb []byte
	eh.safe(func() {
		job2_yb, eh.err = yaml.Marshal(job)
	})
	log.Info("Marshal job to YAML", "cmd", cmd_s, "size", len(job2_yb))
	//	msg2 := fmt.Sprintf("MarshalIndent /job/%s: %d bytes ...",
	//		cmd_s, len(job2_yb))
	//	fmt.Println(msg2)
	//		job.Root.CmdLet += msg2

	timeStamp := "" // fmt.Sprintf("@ %[4]v", time.Now())
	jobScript_b := []byte(fmt.Sprintf(`#!/bin/bash
#
# generated script - do not edit
#
cat <<EOYD | less
#
# begin:  CoLiGui job configuration for:  %[1]s - %[2]s  %[4]s
#

%[3]s
#
# end:  CoLiGui job configuration for:  %[1]s - %[2]s  %[4]s
#
EOYD
`, job.Root.Label, job.Name, job2_yb, timeStamp))

	cmdFName := strings.TrimSpace(strings.ToLower(job.Root.Label))
	cmdDir := filepath.Join(baseDir, cmdFName)
	os.MkdirAll(cmdDir, 0777)

	jobName := strings.TrimSpace(strings.ToLower(job.Name))
	log.Info("generated job script",
		"cmd", cmd_s, "cmdFName", cmdFName, "cmdDir", cmdDir,
		"jobName", jobName, "size", len(jobScript_b))

	var jobFPath, cs string
	eh.safe(func() {
		cs = eh.hashSha1(jobScript_b, nil)[:6]
		jobFName := cmdFName + "-" + jobName + "." + cs + ".cgs"
		jobFPath = filepath.Join(cmdDir, jobFName)
	})

	haveToSaveJob := true

	fInfo, err := os.Stat(jobFPath)
	if err == nil && !fInfo.IsDir() {
		var oldJob_b []byte
		eh.safe(func() { oldJob_b, eh.err = ioutil.ReadFile(jobFPath) })

		haveToSaveJob = bytes.Compare(jobScript_b, oldJob_b) != 0
	}

	cmdMsg := "# job already known, not saved: " + jobFPath //-job.Root.CmdLet
	if haveToSaveJob {
		eh.safe(func() { eh.err = ioutil.WriteFile(jobFPath, jobScript_b, 0777) })
		cmdMsg = "# job saved as: " + jobFPath
	}

	//	eh.safe(func() {
	eh.safe(func() {
		eh.forAllJobs(
			baseDir, cmdFName, jobName, "."+cs, // toIgnore,

			// cmdDir, cmdName, jobName
			func(oldJobFPath string, oldJob_b []byte) error {
				eh.renameToBak(cmdDir, cmdFName, jobName, oldJobFPath, oldJob_b)
				return eh.err
			},
		)
	})
	//	})

	eh.safe(func() {
		res := gin.H{
			"job_name": job.Name,
			"json_id":  job.JsonSha1,
			"yaml_id":  job.YamlSha1,
			"cmd":      cmdMsg, // job.Root.CmdLet,
		}
		c.JSON(http.StatusCreated, res)
	})

	eh.ifErr(func() { c.AbortWithError(http.StatusBadRequest, eh.err) })
	return eh.err
}

func (eh *errHandler_T) renameToBak(
	cmdDir, cmdName, jobName, oldJobFPath string,
	oldJob_b []byte,
) {
	cs := eh.hashSha1(oldJob_b, nil) //--[:6]
	bakJobFPath := mkJobFPath(cmdDir, cmdName, jobName, "."+cs, true)
	eh.safe(func() { eh.err = os.Rename(oldJobFPath, bakJobFPath) })
}

func (eh *errHandler_T) forAllJobs(
	baseDir, cmdName string,
	jobName string,
	toIgnore string,
	handleFile func(string, []byte) error,
) {
	cmdFName := strings.ToLower(strings.TrimSpace(cmdName))
	cmdDir := filepath.Join(baseDir, cmdFName)
	os.MkdirAll(cmdDir, 0777)

	getJobFPath := func(pat string) string {
		return mkJobFPath(cmdDir, cmdFName, jobName, pat, false)
		//		return mkJobFPath(cmdDir, cmdName, "*", pat, false)
	}
	jobFPathPat := getJobFPath("*")
	jobFPathIgn := getJobFPath(toIgnore)

	var foundFiles_l []string
	foundFiles_l, eh.err = filepath.Glob(jobFPathPat)
	if len(foundFiles_l) > 0 {
		if len(foundFiles_l) > 1 {
			fmt.Println("found multiple job files")
		}
		for _, oldJobFPath := range foundFiles_l {
			if oldJobFPath == jobFPathIgn {
				continue
			}

			var oldJob_b []byte
			eh.safe(func() { oldJob_b, eh.err = ioutil.ReadFile(oldJobFPath) })
			eh.safe(func() { eh.err = handleFile(oldJobFPath, oldJob_b) })
		}
	}
}

func (eh *errHandler_T) hashSha1(
	obj interface{},
	marshal func(interface{}) ([]byte, error),
	//	size int,
) (sha1Hash string) {
	var buf_b []byte
	if marshal == nil {
		buf_b = obj.([]byte)
	} else {
		eh.safe(func() { buf_b, eh.err = marshal(obj) })
	}

	h := sha1.New()
	eh.safe(func() { _, eh.err = h.Write(buf_b) })
	eh.safe(func() { sha1Hash = hex.EncodeToString(h.Sum(nil)) })
	return sha1Hash
}

func mkJobFPath(cmdDir, cmdName, jobName, toIgnore string, subdir bool) string {
	jobFNameX := cmdName + "-" + jobName + toIgnore + ".cgs"
	path_l := []string{cmdDir}
	if subdir {
		path_l = append(path_l, jobName)
	}
	jobFDirX := filepath.Join(path_l...)
	os.MkdirAll(jobFDirX, 0777)

	path_l = append(path_l, jobFNameX)
	jobFPathX := filepath.Join(path_l...)
	return jobFPathX
}
