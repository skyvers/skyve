package sail.admin.pf;

import org.junit.Ignore;
import org.junit.Test;

@Ignore
public class PrimeFacesConsolidatedTest extends PrimeFacesTestPreparation {

	
	@Test
	public void test() throws Exception {
		
		String clearTextPassword = "Password01";
		
		login("demo", "admin", clearTextPassword);

		testMenuDevOpsDataMaintenance();
		testMenuJobs();
	}

}
