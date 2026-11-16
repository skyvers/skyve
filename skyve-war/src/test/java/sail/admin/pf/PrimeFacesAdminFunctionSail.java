package sail.admin.pf;

import org.junit.Ignore;
import org.junit.Test;

@Ignore
@SuppressWarnings("java:S3577") // deliberately not named as a test, so neither surefire nor failsafe picks up this ignored script
public class PrimeFacesAdminFunctionSail extends TestAdminMenuExternalTabletAsAdmininstrator {

	@Test
	public void test() {
		
		String clearTextPassword = "admin";
		
		login("admin", clearTextPassword);
		
//		testMenuPassword(clearTextPassword);
//		testMenuUserDashboard();
		testMenuSecurityAdminGroups();
		
//		testMenuContacts();

		logout();
	}
}
