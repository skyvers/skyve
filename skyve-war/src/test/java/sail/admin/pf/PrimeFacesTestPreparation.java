package sail.admin.pf;

import org.junit.After;
import org.junit.Before;

import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;
import util.sail.Devices;
import util.sail.PrimeFacesSelenium;

public class PrimeFacesTestPreparation extends PrimeFacesSelenium {

	@Before
	public void setup() throws Exception {
		startBrowser(new BrowserConfiguration()
							.browser(Browsers.chrome)
							.baseUrl("http://localhost:8080/skyve/")
							.userAgentString(Devices.ipad.userAgentString));
	}
	
	@After
	public void teardown() {
		stopBrowser();
	}

	/**
	 * Menu DevOps::Data Maintenance
	 */
	protected void testMenuDevOpsDataMaintenance() throws Exception {
		trace("Edit new document [admin.DataMaintenance] instance");
		get("?a=e&m=admin&d=DataMaintenance");
		trace("click tab [Backup/Restore]");
		tab("s3:s51");
		trace("click [Backup] (s3:s89) if it exists and is not disabled");
		button("s3:s89", true, false);
		trace("Test Success");
		verifySuccess();
	}

	/**
	 * Menu Jobs
	 */
	protected void testMenuJobs() throws Exception {
		trace("Edit new document [admin.Jobs] instance");
		get("?a=e&m=admin&d=Jobs");
		trace("click tab [Actual]");
		tab("s3:s5");
		trace("click [Refresh] (s47) if it exists and is not disabled");
		button("s47", true, false);
		trace("Test Success");
		verifySuccess();
	}

}
