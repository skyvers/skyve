For PrimeReact

1) download sigma project from prime at https://github.com/primefaces/sigma
2) extract to a folder, referred to as PROJECT_HOME from now on.
3) Add the import 
	import {Router} from './skyve/Router.js';
	to PROJECT_HOME/src/Apps.js at the end of all other imports
4) Add
	menu: []
	to PROJECT_HOME/src/App.js state variable
5) Replace PROJECT_HOME/src/Apps.js createMenu method with
    createMenu() {
    	var me = this;
    	Router.createAppMenu(function(menu) {
    		me.setState({menu: menu});
    		console.log(menu);
    	});
    }
6) Replace 
	<AppMenu model={this.menu} onMenuItemClick={this.onMenuItemClick} />
	with
	<AppMenu model={this.state.menu} onMenuItemClick={this.onMenuItemClick} />
6) Replace PROJECT_HOME/src/Apps.js <div className="layout-main"> in the render method with
                <div className="layout-main">
					<Router />
                </div>
7) Add
	<link type="text/css" rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.css" />
	to PROJECT_HOME/public/index.html so font-awesome will work.
8) Add
	    "proxy": {"/": {"target": "http://localhost:8080", "changeOrigin": true, "followRedirects": true}}
	at the end of PROJECT_HOME/package.json to enable proxying to wildfly.
9) Copy /skyve-ee/src/js/react/** to PROJECT_HOME/src/skyve/
10) npm install
11) Generate react into the PROJECT_HOME (which will create PROJECT_HOME/src/skyve/views/)
12) npm start
