package sail.admin;

import org.junit.Test;

public class AdminFunctionSail extends AdminSail {
	@Test
	public void test() throws Exception {
		login("demo", "admin", "admin");
		
		testMenuPassword();
		testMenuUserDashboard();
		testMenuContacts();
		
		//problem with dynamic domain
		testMenuCommunications(); 
		
		testMenuSecurityAdminGroups();
		testMenuSecurityAdminDataGroups();
		
		//fails on dynamic domain
		testMenuDevOpsDataMaintenance();
		
		testMenuDevOpsDocumentCreator();
		testMenuSnapshots();

		testMenuSystemDashboard();
		
		//fails with error
		testMenuDocumentNumbers();
		
		testMenuJobs();
	}
}
