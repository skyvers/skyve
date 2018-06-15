package sail.admin;

import org.junit.Test;
import org.junit.runner.RunWith;

import util.JUnitMultiThreadedRunner;

@RunWith(JUnitMultiThreadedRunner.class)
public class AdminSailPerformanceTest extends AdminSailTest {
	@Test
	public void test1() throws Exception {
		Thread.sleep(1000);
		login("demo", "admin", "admin");
		while (true) {
			testMenuPassword();
		}
	}
	
	@Test
	public void test2() throws Exception {
		Thread.sleep(2000);
		login("demo", "admin", "admin");
		while (true) {
			testMenuUserDashboard();
		}
	}

	@Test
	public void test3() throws Exception {
		Thread.sleep(3000);
		login("demo", "admin", "admin");
		while (true) {
			testMenuContacts();
		}
	}

	@Test
	public void test4() throws Exception {
		Thread.sleep(4000);
		login("demo", "admin", "admin");
		while (true) {
			testMenuCommunications();
		}
	}	
	@Test
	public void test5() throws Exception {
		Thread.sleep(5000);
		login("demo", "admin", "admin");
		while (true) {
			testMenuSecurityAdminGroups();
		}
	}
	@Test
	public void test6() throws Exception {
		Thread.sleep(6000);
		login("demo", "admin", "admin");
		while (true) {
			testMenuSecurityAdminDataGroups();
		}
	}

	@Test
	public void test7() throws Exception {
		Thread.sleep(7000);
		login("demo", "admin", "admin");
		while (true) {
			testMenuDevOpsDataMaintenance();
		}
	}

	@Test
	public void test8() throws Exception {
		Thread.sleep(8000);
		login("demo", "admin", "admin");
		while (true) {
			testMenuDevOpsDocumentCreator();
		}
	}

	@Test
	public void test9() throws Exception {
		Thread.sleep(9000);
		login("demo", "admin", "admin");
		while (true) {
			testMenuSnapshots();
		}
	}

	@Test
	public void test10() throws Exception {
		Thread.sleep(10000);
		login("demo", "admin", "admin");
		while (true) {
			testMenuSystemDashboard();
		}
	}

	@Test
	public void test11() throws Exception {
		Thread.sleep(11000);
		login("demo", "admin", "admin");
		while (true) {
			testMenuDocumentNumbers();
		}
	}
	
	@Test
	public void test12() throws Exception {
		Thread.sleep(12000);
		login("demo", "admin", "admin");
		while (true) {
			testMenuJobs();
		}
	}
}
