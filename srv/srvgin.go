package srv

import (
	"bytes"
	"crypto/sha1"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	//	"log"
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

func ServeGin(port int) error {
	baseDir := "/tmp"
	//	eh := errHandler_T{}
	return /*eh.*/ ServeGinX(port, baseDir)
}

func /*(eh *errHandler_T)*/ ServeGinX(port int, baseDir string) error {
	router := gin.Default()

	router.GET("/", func(c *gin.Context) {
		wui.WriteWuiHtml(c.Writer)
	})

	router.POST("/jobs/:cmd", func(c *gin.Context) {
		eh := errHandler_T{}
		eh.handleJobPost(baseDir, c)
	})

	router.GET("/jobs", func(c *gin.Context) {
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
	//	cmdName := c.Param("cmd")
	//		id_s := c.Param("id")

	//... parse JSON in post body
	defer c.Request.Body.Close()

	eh.safe(func() {
		res := gin.H{
			//			"Job": //-[]gin.H{
			//			gin.H{
			//				"name": "hra",
			//				"id":   "x1",
			//				"versions": []string{
			//					"v1", "v2", "v3",
			//				},
			//			},

			"JobType": gin.H{
				"Name": "RSync",
				"Id":   "x0",
				"Jobs": []gin.H{
					gin.H{
						"name": "hra",
						"id":   "x1",
						"versions": []string{
							"v1", "v2", "v3",
						},
					},
					gin.H{
						"name": "kati",
						"id":   "x2",
						"versions": []string{
							"v1", "v2", "v3",
						},
					},
					gin.H{
						"name": "default",
						"id":   "x3",
						"versions": []string{
							"v1", "v2", "v3",
						},
					},
				},
			},
			//				"id-x1": "hra",
			//				"id-x2": "kati",
			//				"id-x3": "def",
			//			},
			//			"jid":   job.JsonSha1,
			//			"yid":   job.YamlSha1,
			//			"cmd":   cmdMsg, // job.Root.CmdLet,
		}
		c.JSON(http.StatusCreated, res)
	})

	//	eh.ifErr(func() { c.AbortWithError(http.StatusBadRequest, eh.err) })
	//	return eh.err

	//	eh.safe(func() {
	//		eh.forAllJobs(
	//			baseDir, cmdName, "*", //-- jobName,
	//			"", //	"."+cs, // toIgnore,

	//			// cmdDir, cmdName, jobName
	//			func(oldJobFPath string, oldJob_b []byte) error {
	//				fmt.Printf("found: '%s': %d bytes\n",
	//					oldJobFPath, len(oldJob_b),
	//				)
	//				//				eh.renameToBak(cmdDir, cmdName, jobName, oldJobFPath, oldJob_b)
	//				return eh.err
	//			},
	//		)
	//	})

	eh.ifErr(func() { c.AbortWithError(http.StatusBadRequest, eh.err) })
	return eh.err
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

	cmdName := strings.TrimSpace(strings.ToLower(job.Root.Label))
	cmdDir := filepath.Join(baseDir, cmdName)
	os.MkdirAll(cmdDir, 0777)

	jobName := strings.TrimSpace(strings.ToLower(job.Name))
	log.Info("generated job script",
		"cmd", cmd_s, "cmdName", cmdName, "cmdDir", cmdDir,
		"jobName", jobName, "size", len(jobScript_b))

	var jobFPath, cs string
	eh.safe(func() {
		cs = eh.hashSha1(jobScript_b, nil)[:6]
		jobFName := cmdName + "-" + jobName + "." + cs + ".cgs"
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
			baseDir, cmdName, jobName, "."+cs, // toIgnore,

			// cmdDir, cmdName, jobName
			func(oldJobFPath string, oldJob_b []byte) error {
				eh.renameToBak(cmdDir, cmdName, jobName, oldJobFPath, oldJob_b)
				return eh.err
			},
		)
	})
	//	})

	eh.safe(func() {
		res := gin.H{
			"jid": job.JsonSha1,
			"yid": job.YamlSha1,
			"cmd": cmdMsg, // job.Root.CmdLet,
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
	cmdDir := filepath.Join(baseDir, cmdName)
	os.MkdirAll(cmdDir, 0777)

	getJobFPath := func(pat string) string {
		return mkJobFPath(cmdDir, cmdName, jobName, pat, false)
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
