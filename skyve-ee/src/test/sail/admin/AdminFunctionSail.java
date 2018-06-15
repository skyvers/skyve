package sail.admin;

import org.junit.Test;

public class AdminFunctionSail extends AdminSail {
	@Test
	public void test() throws Exception {
		login("demo", "admin", "admin");
		
		trace("List for default query of [admin.User]");
		get("?a=l&m=admin&q=User");
		trace("Select on row 1 on list grid [admin.User] (s0)");
		listGridSelect("s0", 1);
		trace("Auto complete with search 'A' on lookup description [contact] (s3:s25)");
		lookupDescription("s3:s25", "C");
		lookupDescription("s3:s25", 0);

		testMenuPassword();
		testMenuUserDashboard();
		testMenuContacts();
		testMenuCommunications();
		testMenuSecurityAdminGroups();
		testMenuSecurityAdminDataGroups();
		testMenuDevOpsDataMaintenance();
		testMenuDevOpsDocumentCreator();
		testMenuSnapshots();
		testMenuSystemDashboard();
		testMenuDocumentNumbers();
		testMenuJobs();
	}
}
