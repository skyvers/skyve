package sail.admin;

import org.junit.Test;

public class ConsolidatedTest extends TestPreparation {

	
	@Test
	public void test() throws Exception {
		
		String clearTextPassword = "Password01";
		
		login("demo", "admin", clearTextPassword);

		testMenuDevOpsDataMaintenance();
		testMenuJobs();
	}

}
