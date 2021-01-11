package sail.admin;

import org.junit.Ignore;
import org.junit.Test;

@Ignore
public class ConsolidatedTest extends TestPreparation {

	
	@Test
	public void test() throws Exception {
		
		String clearTextPassword = "Password01";
		
		login("demo", "admin", clearTextPassword);

		testMenuDevOpsDataMaintenance();
		testMenuJobs();
	}

}
