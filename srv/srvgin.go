package srv

import (
	"fmt"
	"io/ioutil"
	"net/http"

	"github.com/gin-gonic/gin"

	"coligui/wui"
)

func ServeGin(port int) {
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
		fmt.Printf("POSTed to /job/%s: '''%s'''\n", cmd_s, body_b)

		c.JSON(200, gin.H{
			"message": "pong",
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
	router.Run(port_s) // listen and server on 0.0.0.0:8080
}
