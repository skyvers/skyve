package sail.admin.pf;

import org.junit.Ignore;
import org.junit.Test;

@Ignore
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
