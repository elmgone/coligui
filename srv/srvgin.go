package srv

import (
	"github.com/gin-gonic/gin"
)

func ServeGin() {
	r := gin.Default()

	//	r.GET("/job/:cmd/:id", func(c *gin.Context) {
	r.POST("/job/:cmd", func(c *gin.Context) {
		//		cmd_s := c.Param("cmd")
		//		//		id_s := c.Param("id")

		//... parse JSON in post body:		c.Request.Body

		c.JSON(200, gin.H{
			"message": "pong",
		})
	})

	r.GET("/ping", func(c *gin.Context) {
		c.JSON(200, gin.H{
			"message": "pong",
		})
	})

	r.Run() // listen and server on 0.0.0.0:8080
}
