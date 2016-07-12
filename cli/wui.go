// Copyright © 2016 ElmGone mrcs.elmgone@mailnull.com
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package cli

import (
	//	"fmt"

	"github.com/spf13/cobra"

	"coligui/srv"
)

var (
	// wuiCmd represents the wui command
	wuiCmd = &cobra.Command{
		Use:   "wui",
		Short: "serve the Web UI",
		Long:  `Run a web server which presents the Web UI for CoLiGui`,
		RunE: func(cmd *cobra.Command, args []string) error {

			baseDir := "/tmp"
			htmlFiles_l := []string{"index.html", "wui/index.html"}
			return srv.ServeGin(33333, baseDir, htmlFiles_l)

			//			srv.ServeWui()
		},
	}
)

func init() {
	RootCmd.AddCommand(wuiCmd)

	// Here you will define your flags and configuration settings.

	// Cobra supports Persistent Flags which will work for this command
	// and all subcommands, e.g.:
	// wuiCmd.PersistentFlags().String("foo", "", "A help for foo")

	// Cobra supports local flags which will only run when this command
	// is called directly, e.g.:
	// wuiCmd.Flags().BoolP("toggle", "t", false, "Help message for toggle")

}
