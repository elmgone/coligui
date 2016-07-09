package srv

import (
	"crypto/sha1"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/gin-gonic/gin"
	"github.com/toqueteos/webbrowser"

	"coligui/wui"
)

type (
	Job struct {
		Id   string
		Name string
		Root Node
	}

	//  { id       : Id
	//  , label    : String
	//  , value    : Value
	//  , kids     : Kids
	//  , isActive : Bool
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

func (eh *errHandler_T) avoidErr(step func() /*error*/) {
	if eh.err == nil {
		/*eh.err =*/ step()
		eh.hasErr(func() {
			log.Printf("ERROR: %s\n", eh.err)
		})
	}
}

func (eh *errHandler_T) hasErr(handle func( /*error*/ )) bool {
	if eh.err != nil {
		handle( /*eh.err*/ )
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
	eh := errHandler_T{}
	return eh.ServeGin(port, baseDir)
}

func (eh *errHandler_T) ServeGin(port int, baseDir string) error {
	router := gin.Default()

	router.GET("/", func(c *gin.Context) {
		wui.WriteWuiHtml(c.Writer)
	})

	router.POST("/job/:cmd", func(c *gin.Context) {
		cmd_s := c.Param("cmd")
		//		id_s := c.Param("id")

		//... parse JSON in post body
		defer c.Request.Body.Close()

		var body_b []byte
		eh.avoidErr(func() {
			body_b, eh.err = ioutil.ReadAll(c.Request.Body)
		})
		msg1 := fmt.Sprintf("POSTed to /job/%s: %d bytes ...", cmd_s, len(body_b))
		fmt.Println(msg1)

		var job Job
		eh.avoidErr(func() {
			eh.err = json.Unmarshal(body_b, &job)
		})
		eh.avoidErr(func() {
			eh.err = job.Check()
			//		fmt.Printf("got '%s': '''%s''' from %#v\n", cmd_s, cmdRes, node)
			//			fmt.Printf("got '%s': %#v: %v\n", cmd_s, job, eh.err)
			fmt.Printf("got '%s': err=%v\n", cmd_s, eh.err)
		})

		var job_b []byte
		eh.avoidErr(func() {
			//			job.Root.CmdLet += msg1
			job_b, eh.err = json.Marshal(job)
		})

		h := sha1.New()
		eh.avoidErr(func() {
			_ /*n*/, eh.err = h.Write(job_b)
		})
		if eh.hasErr(func() {
			c.AbortWithError(http.StatusBadRequest, eh.err)
		}) {
			return
		}

		job.Id = hex.EncodeToString(h.Sum(nil))

		cmdName := strings.TrimSpace(strings.ToLower(job.Root.Label))
		cmdDir := filepath.Join(baseDir, cmdName)
		os.MkdirAll(cmdDir, 0777)

		jobName := strings.TrimSpace(strings.ToLower(job.Name))

		jobFName := cmdName + "-" + jobName + ".cgs"
		jobFPath := filepath.Join(cmdDir, jobFName)

		fInfo, err := os.Stat(jobFPath)
		if err == nil && !fInfo.IsDir() {
			var oldJob_b []byte
			eh.avoidErr(func() {
				oldJob_b, eh.err = ioutil.ReadFile(jobFPath)
			})

			var oldJob Job
			eh.avoidErr(func() {
				eh.err = json.Unmarshal(oldJob_b, &oldJob)
			})

			if job.Id != oldJob.Id {
				oldNode := &oldJob.Root
				eh.avoidErr(func() {
					eh.err = oldNode.ProcessTree()
				})

				oldJobFName := jobFName + "." + oldJob.Id
				oldJobFDir := filepath.Join(cmdDir, jobName)
				os.MkdirAll(oldJobFDir, 0777)
				oldJobFPath := filepath.Join(oldJobFDir, oldJobFName)
				eh.avoidErr(func() {
					eh.err = os.Rename(jobFPath, oldJobFPath)
				})
			}
		}

		var node_ijb []byte
		eh.avoidErr(func() {
			node_ijb, eh.err = json.MarshalIndent(job, "", "  ")
		})
		msg2 := fmt.Sprintf("MarshalIndent /job/%s: %d bytes ...",
			cmd_s, len(node_ijb))
		fmt.Println(msg2)
		//		job.Root.CmdLet += msg2

		eh.avoidErr(func() {
			eh.err = ioutil.WriteFile(jobFPath, node_ijb, 0777)
		})

		eh.avoidErr(func() {
			res := gin.H{
				"id":  job.Id,
				"cmd": job.Root.CmdLet,
			}
			c.JSON(http.StatusCreated, res)
		})

		eh.hasErr(func() {
			c.AbortWithError(http.StatusBadRequest, eh.err)
		})
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
			log.Printf("FAILED to open url in browser: %s\n", err)
		}
	}()

	return router.Run(port_s) // listen and server on 0.0.0.0:8080
}

//func (node *Node) ProcessNode(cmdletsById_m cmdletsById_M) string {
//	//	if !node.IsActive {
//	if node.IsActive != nil && !*node.IsActive {
//		log.Printf("ProcessNode(%s:%s): SKIP\n", node.Type, node.Id)
//		return ""
//	}

//	cmdFmt := node.CmdFmt
//	values_l := make([]interface{}, 0, len(node.Kids)+2)
//	if len(node.Kids) == 0 {
//		if cmdFmt == "" {
//			switch val := node.Value.(type) {
//			case bool:
//				panic("MISSING fmt for Bool value " + node.Id)
//			case string:
//				if val == "" {
//					return ""
//				}
//			default:
//				panic(fmt.Sprintf("UNKNOWN DATA TYPE : %T (%#v)", val, val))
//			}

//			//			if node.Type == "Bool" {
//			//				panic("MISSING fmt for Bool value " + node.Id)
//			//			} //--else {
//			cmdFmt = "%v"
//			//			}
//		}
//		values_l = append(values_l, node.Value)
//		//		cmdlet := fmt.Sprintf(cmdFmt, node.Value)
//		//		cmdletsById_m[node.Id] = cmdlet

//		//		return cmdlet
//	} else {
//		if cmdFmt == "" {
//			cf_l := make([]string, 0, len(node.Kids)+2)
//			//			cf_l[0] = "%[1]v"
//			iKid := 1
//			for _ /*i*/, k := range node.Kids {
//				if !k.IsActive {
//					continue
//				}
//				cf_l = append(cf_l, fmt.Sprintf("%%[%d]v", iKid)) //-- i+1)) //--, k.Value))
//				kVal, ok := cmdletsById_m[k.Id]
//				if !ok {
//					panic("Could not find ID " + k.Id)
//				}
//				values_l = append(values_l, kVal)
//				iKid++
//			}
//			cmdFmt = strings.Join(cf_l, " ")
//			//		}
//		}
//	}

//	cmdlet := fmt.Sprintf(cmdFmt, values_l...)
//	if node.Type == "Bool" {
//		isTrue := node.Value.(bool)
//		if isTrue {
//			cmdlet = cmdFmt
//		} else {
//			cmdlet = ""
//		}
//	}
//	log.Printf("ProcessNode(%s:%s): c=''%s'', c1=''%s'', c0=''%s'', vs=%v\n",
//		node.Type, node.Id, cmdlet, cmdFmt, node.CmdFmt, values_l)
//	cmdletsById_m[node.Id] = cmdlet

//	return cmdlet
//}

//func (node *Node) ProcessNode(cmdletsById_m cmdletsById_M) string {
//	cmdFmt := node.CmdFmt
//	if len(node.Kids) == 0 {
//		if cmdFmt == "" {
//			cmdFmt = "%v"
//		}
//		cmdlet := fmt.Sprintf(cmdFmt, node.Value)
//		cmdletsById_m[node.Id] = cmdlet

//		return cmdlet
//	}

//	if cmdFmt == "" {
//		cf_l := make([]string, 0, len(node.Kids))
//		for i, k := range node.Kids {
//			cf_l = append(cf_l, fmt.Sprintf("%%[%d]v", i+1))
//		}
//		cmdFmt = strings.Join(cf_l, " ")
//	}
//	cmdlet := fmt.Sprintf(cmdFmt, node.Value)
//	cmdletsById_m[node.Id] = cmdlet

//	return cmdlet
//}
