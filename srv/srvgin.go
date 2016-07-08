package srv

import (
	"crypto/sha1"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	//	"strings"
	"errors"
	"time"

	"github.com/gin-gonic/gin"
	"github.com/toqueteos/webbrowser"

	"coligui/wui"
)

type (
	//  { id       : Id
	//  , label    : String
	//  , value    : Value
	//  , kids     : Kids
	//  , isActive : Bool
	Node struct {
		Id     string
		Type   string
		Label  string
		Value  interface{}
		CmdLet string
		Kids   []*Node
		//		IsActive *bool `json:"active"`
	}

	nodesById_M map[string]*Node

//	cmdletsById_M map[string]string
)

func (node *Node) ProcessTree( //--nodesById_m nodesById_M,
//	activate bool,
//) (nodesById_M, cmdletsById_M) {
//) /*string*/ {
) error {
	nodesById_m := make(nodesById_M)
	//	cmdletsById_m := make(cmdletsById_M)

	cnf := func(n *Node) error {
		return n.CheckNode(nodesById_m)
		//		err:=n.CheckNode(nodesById_m)
		//		//		n.ProcessNode(cmdletsById_m)
		//		if err != nil {
		//			return err
		//		}
	}

	return node.WalkTree( //--true, //--activate,
		cnf)

	//	return nodesById_m, cmdletsById_m
	//	return cmdletsById_m[node.Id]
}

func (node *Node) CheckNode(nodesById_m nodesById_M) error { //--}, activate bool) {
	altNode, ok := nodesById_m[node.Id]
	if ok {
		//		panic("Duplicate ID")
		errMsg := fmt.Sprintf("Duplicate ID '%s':  %+v  <->  %+v",
			node.Id, altNode, node)
		return errors.New(errMsg)
	}
	nodesById_m[node.Id] = node
	return nil
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

//func (node *Node) WalkTree(nodesById_m nodesById_M, activate bool) {
func (node *Node) WalkTree( //--activate bool,
	cnf func(*Node) error) error {
	//	node.IsActive = activate
	//	cnf(node)

	for _, kid := range node.Kids {
		//		kidActive := activate
		//		if activate && node.Type == "Switch" {
		//			sId := node.Value.(string)
		//			kidActive = (kid.Id == sId)
		//		}

		//		kid.WalkTree(kidActive, cnf)
		err := kid.WalkTree(cnf)
		if err != nil {
			return err
		}
	}

	//	node.IsActive = activate
	return cnf(node)
}

func ServeGin(port int) error {
	router := gin.Default()

	router.GET("/", func(c *gin.Context) {
		wui.WriteWuiHtml(c.Writer)
	})

	//	r.GET("/job/:cmd/:id", func(c *gin.Context) {
	router.POST("/job/:cmd", func(c *gin.Context) {
		cmd_s := c.Param("cmd")
		//		id_s := c.Param("id")

		//... parse JSON in post body
		defer c.Request.Body.Close()
		body_b, err := ioutil.ReadAll(c.Request.Body)
		if err != nil {
			c.AbortWithError(http.StatusBadRequest, err)
			return
		}
		//		fmt.Printf("POSTed to /job/%s: '''%s'''\n", cmd_s, body_b)
		//		c.String(http.StatusOK, `"%s"`, body_b)

		//		c.JSON(200, gin.H{
		//			"id":  "x1",
		//			"cmd": "bla",
		//		})

		var node Node //- gin.H
		err = json.Unmarshal(body_b, &node)
		if err != nil {
			c.AbortWithError(http.StatusBadRequest, err)
			return
		}
		//		cmdRes :=
		err = node.ProcessTree()
		//		fmt.Printf("got '%s': '''%s''' from %#v\n", cmd_s, cmdRes, node)
		fmt.Printf("got '%s': %#v: %s\n", cmd_s, node, err)
		if err != nil {
			c.AbortWithError(http.StatusBadRequest, err)
			return
		}

		cmdRes := node.CmdLet
		fmt.Printf("got '%s': '''%s''' from %#v\n", cmd_s, cmdRes, node)

		node_b, err := json.Marshal(node)
		if err != nil {
			c.AbortWithError(http.StatusBadRequest, err)
			return
		}

		h := sha1.New()
		_ /*n*/, err = h.Write(node_b)
		if err != nil {
			c.AbortWithError(http.StatusBadRequest, err)
			return
		}

		res := gin.H{
			"id":  hex.EncodeToString(h.Sum(nil)),
			"cmd": cmdRes, //--node.ProcessTree(), //--  "bla",
		}
		c.JSON(http.StatusCreated, res)
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
			//			return err
			log.Printf("FAILED to open url in browser: %s\n", err)
		}
	}()

	return router.Run(port_s) // listen and server on 0.0.0.0:8080
}
