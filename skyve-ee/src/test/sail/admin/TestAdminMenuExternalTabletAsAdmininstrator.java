package sail.admin;

import org.junit.After;
import org.junit.Before;
import org.openqa.selenium.By;

import util.sail.BrowserConfiguration;
import util.sail.Devices;
import util.sail.PrimeFacesTest;

public class TestAdminMenuExternalTabletAsAdmininstrator extends PrimeFacesTest {

	private String pathToChromeDriver = "C:/Users/RBB/chromedriver.exe";
	
	@Before
	public void setup() throws Exception {
		setupChrome(new BrowserConfiguration().baseUrl("http://localhost:8080/skyve/").pathToDriver(pathToChromeDriver).userAgentString(Devices.ipad.userAgentString));
	}
	
	@After
	public void teardown() {
		tearDownBrowser();
	}

	protected void login(String customer, String username, String password) throws Exception {

		driver.get(baseUrl);

		driver.findElement(By.name("customer")).clear();
		driver.findElement(By.name("customer")).sendKeys(customer);

		driver.findElement(By.name("user")).clear();
		driver.findElement(By.name("user")).sendKeys(username);

		driver.findElement(By.name("password")).clear();
		driver.findElement(By.name("password")).sendKeys(password);

		driver.findElement(By.cssSelector("input[type=\"submit\"]")).click();
	}
	
	/**
	 * Menu Password
	 */
	protected void testMenuPassword(String password) throws Exception {
		trace("Edit new document [admin.ChangePassword] instance");
		get("?a=e&m=admin&d=ChangePassword");
		trace("set oldPassword (s7password) if it exists and is not disabled");
		text("s7password", password);
		trace("set newPassword (s14password) if it exists and is not disabled");
		text("s14password", "Curabitur varius a");
		trace("set confirmPassword (s21password) if it exists and is not disabled");
		text("s21password", "Curabitur varius a");
		trace("click [MakePasswordChange] (s32) if it exists and is not disabled");
		button("s32", true, false);
		
		trace("Edit new document [admin.ChangePassword] instance");
		get("?a=e&m=admin&d=ChangePassword");
		trace("set oldPassword (s7password) if it exists and is not disabled");
		text("s7password", "Curabitur varius a");
		trace("set newPassword (s14password) if it exists and is not disabled");
		text("s14password", password);
		trace("set confirmPassword (s21password) if it exists and is not disabled");
		text("s21password", password);
		trace("click [MakePasswordChange] (s32) if it exists and is not disabled");
		button("s32", true, false);

		//and reset password to original password
		
		trace("Test Success");
		verifySuccess();
	}

	/**
	 * Menu User Dashboard
	 */
	protected void testMenuUserDashboard() throws Exception {
		trace("Edit new document [admin.UserDashboard] instance");
		get("?a=e&m=admin&d=UserDashboard");
		trace("set currentUser.userName (s11) if it exists and is not disabled");
		text("s11", "admin");
		trace("set currentUser.contact.name (s18) if it exists and is not disabled");
		text("s18", "admin");
		trace("set currentUser.contact.email1 (s25) if it exists and is not disabled");
		text("s25", "admin@skyve.org");
		trace("set currentUser.contact.mobile (s32) if it exists and is not disabled");
		text("s32", "0444 444 444");
		trace("set groupMembershipList (s39) if it exists and is not disabled");
		text("s39",
				"Vivamus vel neque pellentesque purus hendrerit finibus quis in magna.  In non pretium erat, at imperdiet neque. Vestibulum tincidunt placerat euismod. Vestibulum vitae diam vel quam tempus suscipit vel at arcu. Vestibulum cursus sollicitudin egestas. Curabitur id pretium nulla. Etiam feugiat ipsum sed tellus vulputate congue. In id est at arcu aliquam venenatis. Curabitur eget orci a mi consequat sodales.  Cras eget ligula diam. In id nisl elit.");
		trace("set lastLogin (s46) if it exists and is not disabled");
		text("s46", "12-Dec-2018 10:46");
		trace("click tab [Jobs]");
		tab("s92:s94");
		trace("click tab [Subscriptions]");
		tab("s92:s106");
		trace("click tab [Jobs]");
		tab("s92:s94");
		trace("click tab [Subscriptions]");
		tab("s92:s106");
		trace("click [UpdateMyDetails] (s76) if it exists and is not disabled");
		button("s76", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Jobs]");
		tab("s92:s94");
		trace("click tab [Subscriptions]");
		tab("s92:s106");
	}

	/**
	 * Menu Contacts
	 */
	protected void testMenuContacts() throws Exception {
		trace("List for default query of [admin.Contact]");
		get("?a=l&m=admin&q=Contact");
		trace("New row on list grid [admin.Contact] (s0)");
		listGridButton("s0", "s0:s8", false);
		trace("set name (s9) if it exists and is not disabled");
		text("s9", "Sed ante est, rutrum vel diam id, vehicula euismod lorem.");
		trace("set contactType (s16) if it exists and is not disabled");
		selectOne("s16", 2);
		trace("set email1 (s24) if it exists and is not disabled");
		text("s24","test@skyve.org");
		trace("set mobile (s31) if it exists and is not disabled");
		text("s31", "0412 631 864");
		trace("click [save] (s55) if it exists and is not disabled");
		button("s55", true, false);
		trace("Test Success");
		verifySuccess();
		trace("set name (s9) if it exists and is not disabled");
		text("s9", "Donec sed feugiat neque. Vestibulum tincidunt placerat euismod. Ut nec bibendum elit, vitae malesuada massa.");
		trace("set contactType (s16) if it exists and is not disabled");
		selectOne("s16", 2);
		trace("set email1 (s24) if it exists and is not disabled");
		text("s24","test2@skyve.org");
		trace("set mobile (s31) if it exists and is not disabled");
		text("s31", "0426 612 418");
		trace("click [save] (s55) if it exists and is not disabled");
		button("s55", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [delete] (s56) if it exists and is not disabled");
		redirectButton("s56", true);
		trace("Test Success");
		verifySuccess();
	}

	/**
	 * Menu Security Admin::Groups
	 */
	protected void testMenuSecurityAdminGroups() throws Exception {
		trace("List for default query of [admin.Group]");
		get("?a=l&m=admin&q=Group");
		trace("New row on list grid [admin.Group] (s0)");
		listGridButton("s0", "s0:s4", false);
		trace("set name (s11) if it exists and is not disabled");
		text("s11", "Aenean ut odi");
		trace("set description (s18) if it exists and is not disabled");
		text("s18", "In sit amet mi eget libero varius pharetra sit amet quis dolor. Sed in ultrices turpis. Nulla aliquam bibendum massa ut tristique.");
		trace("New row on data grid [roles] (s24)");
		dataGridButton("s24", "s24:s33", false);
		trace("set roleName (s7) if it exists and is not disabled");
		selectOne("s7", 7);
		trace("click [zoom out] (s17) if it exists and is not disabled");
		button("s17", false, false);
		trace("Test Success");
		assertSuccess();
		trace("click [save] (s46) if it exists and is not disabled");
		button("s46", true, false);
		trace("Test Success");
		assertSuccess();
		trace("New row on data grid [roles] (s24)");
		dataGridButton("s24", "s24:s33", false);
		trace("set roleName (s7) if it exists and is not disabled");
		selectOne("s7", 14);
		trace("click [zoom out] (s17) if it exists and is not disabled");
		button("s17", false, false);
		trace("Test Success");
		assertSuccess();
		trace("click [save] (s46) if it exists and is not disabled");
		button("s46", true, false);
		trace("Test Success");
		assertSuccess();
		trace("set name (s11) if it exists and is not disabled");
		text("s11", "Maecenas vestibulum lacus i");
		trace("set description (s18) if it exists and is not disabled");
		text("s18", "Vestibulum et ex et ligula tincidunt pharetra. In sit amet mi eget libero varius pharetra sit amet quis dolor.");
		trace("click [save] (s46) if it exists and is not disabled");
		button("s46", true, false);
		trace("Test Success");
		assertSuccess();
		trace("Remove on row 0 on data grid [roles] (s24)");
		dataGridButton("s24", "s24:0:s36", false);		
		trace("click [save] (s15) if it exists and is not disabled");
		button("s15", true, false);
		trace("Test Success");
		assertSuccess();
		trace("click [delete] (s16) if it exists and is not disabled");
		redirectButton("s16", true);
		trace("Test Success");
		assertSuccess();
	}
	
	/**
	 * Menu Security Admin::Users
	 */
	protected void testMenuSecurityAdminUsers() throws Exception {
		trace("Edit new document [admin.UserList] instance");
		get("?a=e&m=admin&d=UserList");
		trace("click tab [Users]");
		tab("s3:s5");
		trace("click tab [Invite Users]");
		tab("s3:s16");
		trace("set userInvitiationEmailList (s3:s34) if it exists and is not disabled");
		text("s3:s34",
				"Aliquam quis lorem in est dignissim semper in vitae mauris. Mauris sagittis. Duis nec tincidunt velit, id vestibulum sem. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos.  Nunc facilisis velit ut tempor vestibulum. Vivamus ac libero et diam consectetur gravida non vitae nisl. Nulla finibus feugiat sapien, at tincidunt nisi pharetra vel. Sed in nulla id nunc consectetur elementum ut non magna. In hac habitasse platea dictumst. Sed quis odio quam. Integer pharetra eget erat non ornare. Aenean ut odio hendrerit, consequat orci a, ultrices massa. Phasellus dapibus mauris in mattis ultrices. Cras venenatis sit amet urna a eleifend. Donec dui nibh, vehicula id scelerisque et, convallis eget nibh. Phasellus facilisis, nibh eu tincidunt porttitor, magna diam aliquam magna, ut tincidunt erat magna pharetra est. In placerat interdum vulputate. Integer pulvinar nunc dui, at blandit eros ultricies eget. Curabitur varius ante quis arcu dignissim, ac elementum nunc pharetra. Curabitur id pretium nulla. Sed ante est, rutrum vel diam id, vehicula euismod lorem. Suspendisse varius sit amet lorem vitae efficitur. Vestibulum tincidunt placerat euismod. Curabitur aliquam accumsan purus, sed ultricies elit pulvinar in.");
		trace("click tab [Users]");
		tab("s3:s5");
		trace("click tab [Invite Users]");
		tab("s3:s16");
		trace("click tab [Users]");
		tab("s3:s5");
		trace("click tab [Invite Users]");
		tab("s3:s16");
		trace("click [InviteUsers] (s3:s39) if it exists and is not disabled");
		button("s3:s39", true, true);
		trace("Test Success");
		verifySuccess();
	}

	/**
	 * Menu Security Admin::Data Groups
	 */
	protected void testMenuSecurityAdminDataGroups() throws Exception {
		trace("List for default query of [admin.DataGroup]");
		get("?a=l&m=admin&q=DataGroup");
		trace("New row on list grid [admin.DataGroup] (s0)");
		listGridButton("s0", "s0:s4", false);
		trace("set name (s7) if it exists and is not disabled");
		text("s7", "Phasellus at vehicula tortor");
		trace("set description (s14) if it exists and is not disabled");
		text("s14",
				"Vestibulum vitae diam vel quam tempus suscipit vel at arcu. Vestibulum cursus sollicitudin egestas. In cursus dignissim est ut varius. Nulla finibus feugiat sapien, at tincidunt nisi pharetra vel.  In non pretium erat, at imperdiet neque. Cras venenatis sit amet urna a eleifend. Vestibulum tincidunt placerat euismod.  Nunc facilisis velit ut tempor vestibulum. Aenean id orci sagittis odio vehicula mollis vel sed risus. Integer pharetra eget erat non ornare. Phasellus dapibus mauris in mattis ultrices. Donec sed feugiat neque. In id est at arcu aliquam venenatis. Vivamus vel neque pellentesque purus hendrerit finibus quis in magna. Quisque lacus purus, suscipit et elit eget, interdum semper nibh. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Nulla aliquam bibendum massa ut tristique. Suspendisse potenti. In sit amet mi eget libero varius pharetra sit amet quis dolor. Curabitur eget orci a mi consequat sodales. Donec dui nibh, vehicula id scelerisque et, convallis eget nibh. Suspendisse ut mi felis. Maecenas vestibulum lacus id tellus eleifend, vel pellentesque ante lobortis.");
		trace("click [save] (s21) if it exists and is not disabled");
		button("s21", true, false);
		trace("Test Success");
		verifySuccess();
		trace("set name (s7) if it exists and is not disabled");
		text("s7", "Vivamus maximus tellus nequ");
		trace("set description (s14) if it exists and is not disabled");
		text("s14",
				"Donec dui nibh, vehicula id scelerisque et, convallis eget nibh. Suspendisse potenti. Donec convallis vitae leo eu hendrerit. Quisque lacus purus, suscipit et elit eget, interdum semper nibh. Vivamus ac libero et diam consectetur gravida non vitae nisl. Cras venenatis sit amet urna a eleifend. Aliquam quis lorem in est dignissim semper in vitae mauris. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Pellentesque pellentesque, lectus at mattis egestas, metus nunc rutrum ipsum, ut imperdiet mi felis sed urna. Suspendisse varius sit amet lorem vitae efficitur. Aenean ut odio hendrerit, consequat orci a, ultrices massa. Fusce posuere laoreet tempus. Suspendisse ut mi felis.");
		trace("click [save] (s21) if it exists and is not disabled");
		button("s21", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [delete] (s22) if it exists and is not disabled");
		redirectButton("s22", true);
		trace("Test Success");
		verifySuccess();
	}

	/**
	 * Menu Security Admin::Configuration
	 */
	protected void testMenuSecurityAdminConfiguration() throws Exception {
		trace("Edit new document [admin.Configuration] instance");
		get("?a=e&m=admin&d=Configuration");
		trace("set passwordComplexityModel (s7) if it exists and is not disabled");
		selectOne("s7", 2);
		trace("set fromEmail (s18) if it exists and is not disabled");
		text("s18",
				"TDEUMUTYFBAZJXHJKYUJBRJAXJRTSIPKAVAMCIGDAANYQMMANBNACIUBJOAUIPVRNVQMCXSRFBEYYHPPSODCRCZKTFZAQJXKVHVWLNLLDKHDKLRHGAMDNXDQMGFJSS@VTHHZVVXWJPLQASBAQYNMXSIBUCRVTRWBSNTCBYAVBKDQJGCQQSWKKULSCADWWAPZTINJBPZEGFWTYODRUQNMJIRWNRUXLEPLNPRVPNCDTNCPVWUDPDITENESALI.WG");
		trace("set passwordResetEmailSubject (s25) if it exists and is not disabled");
		text("s25", "Etiam feugiat ipsum sed tellus vulputate congue. Vivamus vel neque pellentesque purus hendrerit finibus quis in magna.");
		trace("set passwordResetEmailBody (s32) if it exists and is not disabled");
		trace("set allowUserSelfRegistration (s49) if it exists and is not disabled");
		checkbox("s49", Boolean.FALSE);
		trace("click [save] (s56) if it exists and is not disabled");
		button("s56", true, false);
		trace("Test Success");
		verifySuccess();
		trace("set passwordComplexityModel (s7) if it exists and is not disabled");
		selectOne("s7", 1);
		trace("set fromEmail (s18) if it exists and is not disabled");
		text("s18",
				"EKSETEHUZLROIRNSCUELEADQJGDOLCFVVOSIEQOYCHOJOFGSXFAFIKRIQCCWAWLIWSJDDLWGZBUBVTBEGUYTIJFALYEZBLLJMIGNVFODMLIQWPTXWQQINNDMACHLVR@QNGNPWNFBDWVHNPPRPFHODDULLNOCNXZASWYJUJHBEBVWPYLUJTTCMBLDVLUCQLONDUIRFYYBVWGAIPKKZEBATTJSEVSHERJQHJXNSOTUVQCCDDESTCVMCJLCLFX.EQ");
		trace("set passwordResetEmailSubject (s25) if it exists and is not disabled");
		text("s25",
				"Cras venenatis sit amet urna a eleifend. Vivamus ac libero et diam consectetur gravida non vitae nisl. Duis nec tincidunt velit, id vestibulum sem. Praesent ut dignissim ligula.");
		trace("set passwordResetEmailBody (s32) if it exists and is not disabled");
		trace("set allowUserSelfRegistration (s49) if it exists and is not disabled");
		checkbox("s49", Boolean.FALSE);
		trace("click [save] (s56) if it exists and is not disabled");
		button("s56", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [save] (s56) if it exists and is not disabled");
		button("s56", true, false);
		trace("Test Success");
		verifySuccess();
	}

	/**
	 * Menu DevOps::Document Creator
	 */
	protected void testMenuDevOpsDocumentCreator() throws Exception {
		trace("Edit new document [admin.DocumentCreator] instance");
		get("?a=e&m=admin&d=DocumentCreator");
		trace("click tab [Input]");
		tab("s3:s5");
		trace("set script (s3:s11) if it exists and is not disabled");
		text("s3:s11",
				"In placerat interdum vulputate. Integer pharetra diam eget felis imperdiet, ut tempus libero porttitor. Donec eget elit neque. Curabitur varius ante quis arcu dignissim, ac elementum nunc pharetra. Duis eget quam vehicula, hendrerit leo ac, ullamcorper augue. Integer pharetra eget erat non ornare. In hac habitasse platea dictumst. Integer pulvinar nunc dui, at blandit eros ultricies eget. Sed quis odio quam.  In non pretium erat, at imperdiet neque. Praesent ut dignissim ligula. Aenean ut odio hendrerit, consequat orci a, ultrices massa. Curabitur varius quis quam a placerat. Phasellus dapibus mauris in mattis ultrices.Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nulla aliquam bibendum massa ut tristique. Etiam feugiat ipsum sed tellus vulputate congue. Nulla finibus feugiat sapien, at tincidunt nisi pharetra vel. Nullam dignissim ac libero vitae aliquam. Donec dui nibh, vehicula id scelerisque et, convallis eget nibh. Suspendisse varius sit amet lorem vitae efficitur. Vivamus porttitor, metus vitae lobortis mattis, libero metus tincidunt velit, nec dapibus ipsum arcu vel odio. Aenean id orci sagittis odio vehicula mollis vel sed risus. Fusce posuere laoreet tempus. Maecenas vestibulum lacus id tellus eleifend, vel pellentesque ante lobortis. Mauris sagittis. Cras venenatis sit amet urna a eleifend. Curabitur eget orci a mi consequat sodales. Duis nec tincidunt velit, id vestibulum sem. Vivamus ac libero et diam consectetur gravida non vitae nisl. Quisque lacus purus, suscipit et elit eget, interdum semper nibh. In sit amet mi eget libero varius pharetra sit amet quis dolor. Sed in nulla id nunc consectetur elementum ut non magna. Mauris posuere erat at velit convallis, ut sodales lacus tristique. Curabitur id pretium nulla. Sed in ultrices turpis. Sed ante est, rutrum vel diam id, vehicula euismod lorem. Curabitur ultrices fermentum lectus a luctus. Vivamus congue libero risus, nec cursus tortor congue eget.");
		trace("set outputLocation (s3:s18) if it exists and is not disabled");
		text("s3:s18", "Donec dui nibh, vehicula id scelerisque et, convallis eget nibh.");
		trace("set defaultModule (s3:s25) if it exists and is not disabled");
		selectOne("s3:s25", 1);
		trace("click tab [Markdown Preview]");
		tab("s3:s32");
		trace("click tab [Document Preview]");
		tab("s3:s48");
		trace("click tab [Help]");
		tab("s3:s56");
		trace("click tab [Input]");
		tab("s3:s5");
		trace("click tab [Markdown Preview]");
		tab("s3:s32");
		trace("click tab [Document Preview]");
		tab("s3:s48");
		trace("click tab [Help]");
		tab("s3:s56");
		trace("click tab [Input]");
		tab("s3:s5");
		trace("click tab [Markdown Preview]");
		tab("s3:s32");
		trace("click tab [Document Preview]");
		tab("s3:s48");
		trace("click tab [Help]");
		tab("s3:s56");
		trace("click [Submit] (s66) if it exists and is not disabled");
		button("s66", true, true);
		trace("Test Success");
		verifySuccess();
	}

	/**
	 * Menu Snapshots
	 */
	protected void testMenuSnapshots() throws Exception {
		trace("List for default query of [admin.Snapshot]");
		get("?a=l&m=admin&q=Snapshot");
		trace("New row on list grid [admin.Snapshot] (s0)");
		listGridButton("s0", "s0:s5", false);
		trace("set moduleName (s7) if it exists and is not disabled");
		text("s7", "Quisque lacus purus, suscipit et elit eget, interdum semper nibh.");
		trace("set queryName (s14) if it exists and is not disabled");
		text("s14", "Integer pulvinar nunc dui, at blandit eros ultricies eget.");
		trace("set name (s21) if it exists and is not disabled");
		text("s21", "Nulla finibus feugiat sapien, at tincidunt nisi pharetra vel. In id nisl elit.");
		trace("set snapshot (s28) if it exists and is not disabled");
		text("s28",
				"Sed ante est, rutrum vel diam id, vehicula euismod lorem. Cras venenatis sit amet urna a eleifend. Vestibulum cursus sollicitudin egestas.  Cras eget ligula diam. Vivamus vel neque pellentesque purus hendrerit finibus quis in magna. Vestibulum vitae diam vel quam tempus suscipit vel at arcu. Curabitur varius ante quis arcu dignissim, ac elementum nunc pharetra. Nullam ullamcorper leo laoreet suscipit scelerisque. In id est at arcu aliquam venenatis. Curabitur varius quis quam a placerat. Sed in ultrices turpis. Praesent ut dignissim ligula. Curabitur aliquam accumsan purus, sed ultricies elit pulvinar in. Phasellus dapibus mauris in mattis ultrices. Curabitur eget orci a mi consequat sodales. Proin nec pharetra orci. Sed quis odio quam. Nulla finibus feugiat sapien, at tincidunt nisi pharetra vel. Pellentesque pellentesque, lectus at mattis egestas, metus nunc rutrum ipsum, ut imperdiet mi felis sed urna. Aenean id orci sagittis odio vehicula mollis vel sed risus. Etiam feugiat ipsum sed tellus vulputate congue. Nulla aliquam bibendum massa ut tristique. Aenean ut odio hendrerit, consequat orci a, ultrices massa. Integer pharetra eget erat non ornare.Lorem ipsum dolor sit amet, consectetur adipiscing elit. Curabitur ultrices fermentum lectus a luctus. Phasellus facilisis, nibh eu tincidunt porttitor, magna diam aliquam magna, ut tincidunt erat magna pharetra est. Ut nec bibendum elit, vitae malesuada massa.  Etiam luctus cursus fermentum. Suspendisse potenti. Vestibulum et ex et ligula tincidunt pharetra. Quisque lacus purus, suscipit et elit eget, interdum semper nibh. In cursus dignissim est ut varius. Mauris sagittis. Suspendisse varius sit amet lorem vitae efficitur. Vivamus maximus tellus neque, nec vestibulum urna gravida et. Quisque varius, sem eu varius molestie, eros augue cursus purus, nec mattis leo mauris vitae nisi.");
		trace("set copyToUserSnapshotName (s45) if it exists and is not disabled");
		text("s45", "In id nisl elit. Mauris sagittis.");
		trace("click [save] (s61) if it exists and is not disabled");
		button("s61", true, false);
		trace("Test Success");
		verifySuccess();
		trace("set moduleName (s7) if it exists and is not disabled");
		text("s7", "Integer pulvinar nunc dui, at blandit eros ultricies eget. Fusce posuere laoreet tempus.");
		trace("set queryName (s14) if it exists and is not disabled");
		text("s14", "Lorem ipsum dolor sit amet, consectetur adipiscing elit.");
		trace("set name (s21) if it exists and is not disabled");
		text("s21",
				"Aenean id orci sagittis odio vehicula mollis vel sed risus. Cras venenatis sit amet urna a eleifend.  Cras eget ligula diam. Vivamus congue libero risus, nec cursus tortor congue eget.");
		trace("set snapshot (s28) if it exists and is not disabled");
		text("s28",
				"Proin nec pharetra orci. Nullam ullamcorper leo laoreet suscipit scelerisque. Vestibulum vitae diam vel quam tempus suscipit vel at arcu. Sed in nulla id nunc consectetur elementum ut non magna. Curabitur ultrices fermentum lectus a luctus. Sed ante est, rutrum vel diam id, vehicula euismod lorem. Curabitur eget orci a mi consequat sodales.  Etiam luctus cursus fermentum. Sed in ultrices turpis. Phasellus facilisis, nibh eu tincidunt porttitor, magna diam aliquam magna, ut tincidunt erat magna pharetra est. Mauris posuere erat at velit convallis, ut sodales lacus tristique. Phasellus dapibus mauris in mattis ultrices. Integer pharetra diam eget felis imperdiet, ut tempus libero porttitor. Aenean id orci sagittis odio vehicula mollis vel sed risus. Vestibulum et ex et ligula tincidunt pharetra. Quisque varius, sem eu varius molestie, eros augue cursus purus, nec mattis leo mauris vitae nisi. In hac habitasse platea dictumst. Ut nec bibendum elit, vitae malesuada massa.");
		trace("set copyToUserSnapshotName (s45) if it exists and is not disabled");
		text("s45", "Vivamus congue libero risus, nec cursus tortor congue eget.");
		trace("click [save] (s61) if it exists and is not disabled");
		button("s61", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [CopySnapshotToUser] (s56) if it exists and is not disabled");
		button("s56", true, true);
		trace("Test Success");
		verifySuccess();
		trace("click [delete] (s62) if it exists and is not disabled");
		redirectButton("s62", true);
		trace("Test Success");
		verifySuccess();
	}

	/**
	 * Menu Tags
	 */
	protected void testMenuTags() throws Exception {
		trace("List for default query of [admin.Tag]");
		get("?a=l&m=admin&q=Tag");
		trace("New row on list grid [admin.Tag] (s0)");
		listGridButton("s0", "s0:s5", false);
		trace("click tab [Details]");
		tab("s3:s5");
		trace("set name (s3:s11) if it exists and is not disabled");
		text("s3:s11", "Donec sed feugiat neque. Sed quis odio quam. Mauris posuere erat at velit convallis, ut sodales lacus tristique.");
		trace("set name (s3:s58) if it exists and is not disabled");
		text("s3:s58", "Donec sed feugiat neque. Sed quis odio quam. Mauris posuere erat at velit convallis, ut sodales lacus tristique.");
		trace("set visible (s3:s18) if it exists and is not disabled");
		checkbox("s3:s18", Boolean.FALSE);
		trace("set copyToUserTagName (s3:s35) if it exists and is not disabled");
		text("s3:s35", "Vivamus vel neque pellentesque purus hendrerit finibus quis in magna.");
		trace("click tab [Combinations]");
		tab("s3:s50");
		trace("set name (s3:s11) if it exists and is not disabled");
		text("s3:s11", "Donec sed feugiat neque. Sed quis odio quam. Mauris posuere erat at velit convallis, ut sodales lacus tristique.");
		trace("set name (s3:s58) if it exists and is not disabled");
		text("s3:s58", "Donec sed feugiat neque. Sed quis odio quam. Mauris posuere erat at velit convallis, ut sodales lacus tristique.");
		trace("set combinationsOperator (s3:s69) if it exists and is not disabled");
		selectOne("s3:s69", 1);
		trace("set actionTag (s3:s77) if it exists and is not disabled");
		selectOne("s3:s77", 0);
		trace("click tab [Load]");
		tab("s3:s110");
		trace("set uploadModuleName (s3:s116) if it exists and is not disabled");
		selectOne("s3:s116", 0);
		trace("set uploadDocumentName (s3:s124) if it exists and is not disabled");
		selectOne("s3:s124", 0);
		trace("set attributeName (s3:s153) if it exists and is not disabled");
		selectOne("s3:s153", 0);
		trace("set filterOperator (s3:s161) if it exists and is not disabled");
		selectOne("s3:s161", 1);
		trace("set filterAction (s3:s169) if it exists and is not disabled");
		selectOne("s3:s169", 2);
		trace("set fileHasHeaders (s3:s177) if it exists and is not disabled");
		checkbox("s3:s177", Boolean.FALSE);
		trace("set filterColumn (s3:s184) if it exists and is not disabled");
		_input("s3:s184", "1190");
		trace("set numberLoaded (s3:s207) if it exists and is not disabled");
		text("s3:s207", "7858");
		trace("set numberMatched (s3:s214) if it exists and is not disabled");
		text("s3:s214", "7467");
		trace("set numberTagged (s3:s221) if it exists and is not disabled");
		text("s3:s221", "382");
		trace("click tab [Action]");
		tab("s3:s227");
		trace("set actionModuleName (s3:s233) if it exists and is not disabled");
		selectOne("s3:s233", 0);
		trace("set actionDocumentName (s3:s241) if it exists and is not disabled");
		selectOne("s3:s241", 0);
		trace("set documentAction (s3:s252) if it exists and is not disabled");
		selectOne("s3:s252", 0);
		trace("set documentCondition (s3:s260) if it exists and is not disabled");
		selectOne("s3:s260", 0);
		trace("set unTagSuccessful (s3:s268) if it exists and is not disabled");
		checkbox("s3:s268", Boolean.FALSE);
		trace("set notification (s3:s278) if it exists and is not disabled");
		checkbox("s3:s278", Boolean.FALSE);
		trace("set documentActionResults (s3:s297) if it exists and is not disabled");
		text("s3:s297",
				"Vivamus porttitor, metus vitae lobortis mattis, libero metus tincidunt velit, nec dapibus ipsum arcu vel odio. Ut nec bibendum elit, vitae malesuada massa. Curabitur varius quis quam a placerat. In hac habitasse platea dictumst. In sit amet mi eget libero varius pharetra sit amet quis dolor. Donec dui nibh, vehicula id scelerisque et, convallis eget nibh. Duis nec tincidunt velit, id vestibulum sem. In placerat interdum vulputate. Curabitur id pretium nulla. Vestibulum et ex et ligula tincidunt pharetra. Vivamus maximus tellus neque, nec vestibulum urna gravida et. Pellentesque pellentesque, lectus at mattis egestas, metus nunc rutrum ipsum, ut imperdiet mi felis sed urna. Vivamus ac libero et diam consectetur gravida non vitae nisl. Phasellus facilisis, nibh eu tincidunt porttitor, magna diam aliquam magna, ut tincidunt erat magna pharetra est. Suspendisse ut mi felis. In cursus dignissim est ut varius. Curabitur aliquam accumsan purus, sed ultricies elit pulvinar in. Nullam ullamcorper leo laoreet suscipit scelerisque. Donec convallis vitae leo eu hendrerit. Nulla finibus feugiat sapien, at tincidunt nisi pharetra vel. Quisque lacus purus, suscipit et elit eget, interdum semper nibh.  In non pretium erat, at imperdiet neque. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Phasellus at vehicula tortor. Donec eget elit neque. Vestibulum vitae diam vel quam tempus suscipit vel at arcu. Vivamus congue libero risus, nec cursus tortor congue eget.");
		trace("click tab [Details]");
		tab("s3:s5");
		trace("click tab [Combinations]");
		tab("s3:s50");
		trace("click tab [Load]");
		tab("s3:s110");
		trace("click tab [Action]");
		tab("s3:s227");
		trace("click [save] (s304) if it exists and is not disabled");
		button("s304", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Details]");
		tab("s3:s5");
		trace("set name (s3:s11) if it exists and is not disabled");
		text("s3:s11",
				"Sed quis odio quam. Curabitur id pretium nulla. Vivamus vel neque pellentesque purus hendrerit finibus quis in magna. In sit amet mi eget libero varius pharetra sit amet quis dolor.");
		trace("set name (s3:s58) if it exists and is not disabled");
		text("s3:s58",
				"Sed quis odio quam. Curabitur id pretium nulla. Vivamus vel neque pellentesque purus hendrerit finibus quis in magna. In sit amet mi eget libero varius pharetra sit amet quis dolor.");
		trace("set visible (s3:s18) if it exists and is not disabled");
		checkbox("s3:s18", Boolean.FALSE);
		trace("set copyToUserTagName (s3:s35) if it exists and is not disabled");
		text("s3:s35",
				"Cras eget ligula diam.  In non pretium erat, at imperdiet neque. Curabitur aliquam accumsan purus, sed ultricies elit pulvinar in. Vivamus maximus tellus neque, nec vestibulum urna gravida et.");
		trace("click tab [Combinations]");
		tab("s3:s50");
		trace("set name (s3:s11) if it exists and is not disabled");
		text("s3:s11",
				"Sed quis odio quam. Curabitur id pretium nulla. Vivamus vel neque pellentesque purus hendrerit finibus quis in magna. In sit amet mi eget libero varius pharetra sit amet quis dolor.");
		trace("set name (s3:s58) if it exists and is not disabled");
		text("s3:s58",
				"Sed quis odio quam. Curabitur id pretium nulla. Vivamus vel neque pellentesque purus hendrerit finibus quis in magna. In sit amet mi eget libero varius pharetra sit amet quis dolor.");
		trace("set combinationsOperator (s3:s69) if it exists and is not disabled");
		selectOne("s3:s69", 3);
		trace("set actionTag (s3:s77) if it exists and is not disabled");
		selectOne("s3:s77", 0);
		trace("click tab [Load]");
		tab("s3:s110");
		trace("set uploadModuleName (s3:s116) if it exists and is not disabled");
		selectOne("s3:s116", 0);
		trace("set uploadDocumentName (s3:s124) if it exists and is not disabled");
		selectOne("s3:s124", 0);
		trace("set attributeName (s3:s153) if it exists and is not disabled");
		selectOne("s3:s153", 0);
		trace("set filterOperator (s3:s161) if it exists and is not disabled");
		selectOne("s3:s161", 2);
		trace("set filterAction (s3:s169) if it exists and is not disabled");
		selectOne("s3:s169", 1);
		trace("set fileHasHeaders (s3:s177) if it exists and is not disabled");
		checkbox("s3:s177", Boolean.FALSE);
		trace("set filterColumn (s3:s184) if it exists and is not disabled");
		_input("s3:s184", "7001");
		trace("set numberLoaded (s3:s207) if it exists and is not disabled");
		text("s3:s207", "1358");
		trace("set numberMatched (s3:s214) if it exists and is not disabled");
		text("s3:s214", "7995");
		trace("set numberTagged (s3:s221) if it exists and is not disabled");
		text("s3:s221", "3096");
		trace("click tab [Action]");
		tab("s3:s227");
		trace("set actionModuleName (s3:s233) if it exists and is not disabled");
		selectOne("s3:s233", 0);
		trace("set actionDocumentName (s3:s241) if it exists and is not disabled");
		selectOne("s3:s241", 0);
		trace("set documentAction (s3:s252) if it exists and is not disabled");
		selectOne("s3:s252", 0);
		trace("set documentCondition (s3:s260) if it exists and is not disabled");
		selectOne("s3:s260", 0);
		trace("set unTagSuccessful (s3:s268) if it exists and is not disabled");
		checkbox("s3:s268", Boolean.FALSE);
		trace("set notification (s3:s278) if it exists and is not disabled");
		checkbox("s3:s278", Boolean.FALSE);
		trace("set documentActionResults (s3:s297) if it exists and is not disabled");
		text("s3:s297",
				"Phasellus at vehicula tortor. Donec eget elit neque. In id est at arcu aliquam venenatis. Suspendisse potenti. Integer pharetra diam eget felis imperdiet, ut tempus libero porttitor. Phasellus dapibus mauris in mattis ultrices. Donec convallis vitae leo eu hendrerit. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Ut nec bibendum elit, vitae malesuada massa. Maecenas vestibulum lacus id tellus eleifend, vel pellentesque ante lobortis.  Nunc facilisis velit ut tempor vestibulum. Vestibulum vitae diam vel quam tempus suscipit vel at arcu. In hac habitasse platea dictumst. Pellentesque pellentesque, lectus at mattis egestas, metus nunc rutrum ipsum, ut imperdiet mi felis sed urna. In placerat interdum vulputate. Vivamus maximus tellus neque, nec vestibulum urna gravida et. Nullam ullamcorper leo laoreet suscipit scelerisque. Curabitur ultrices fermentum lectus a luctus. Curabitur eget orci a mi consequat sodales. Integer pulvinar nunc dui, at blandit eros ultricies eget. In id nisl elit. Sed quis odio quam. Curabitur aliquam accumsan purus, sed ultricies elit pulvinar in. Nullam dignissim ac libero vitae aliquam. Quisque lacus purus, suscipit et elit eget, interdum semper nibh. Duis nec tincidunt velit, id vestibulum sem. Vivamus porttitor, metus vitae lobortis mattis, libero metus tincidunt velit, nec dapibus ipsum arcu vel odio.  In non pretium erat, at imperdiet neque. Nulla risus libero, pharetra ut nisi luctus, commodo vestibulum velit. Sed ante est, rutrum vel diam id, vehicula euismod lorem. Nulla aliquam bibendum massa ut tristique. Proin nec pharetra orci. Aliquam quis lorem in est dignissim semper in vitae mauris. Fusce posuere laoreet tempus.");
		trace("click [save] (s304) if it exists and is not disabled");
		button("s304", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Details]");
		tab("s3:s5");
		trace("click [CopyTagToUser] (s3:s46) if it exists and is not disabled");
		button("s3:s46", true, true);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Combinations]");
		tab("s3:s50");
		trace("click [PerformCombination] (s3:s106) if it exists and is not disabled");
		button("s3:s106", true, true);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Load]");
		tab("s3:s110");
		trace("click [Clear] (s3:s136) if it exists and is not disabled");
		button("s3:s136", true, true);
		trace("Test Success");
		verifySuccess();
		trace("click [TagAll] (s3:s145) if it exists and is not disabled");
		button("s3:s145", true, true);
		trace("Test Success");
		verifySuccess();
		trace("click [UploadTagCriteria] (s3:s198) if it exists and is not disabled");
		button("s3:s198", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Action]");
		tab("s3:s227");
		trace("click [BulkDocumentAction] (s3:s292) if it exists and is not disabled");
		button("s3:s292", true, true);
		trace("Test Success");
		verifySuccess();
		trace("click [delete] (s305) if it exists and is not disabled");
		redirectButton("s305", true);
		trace("Test Success");
		verifySuccess();
	}

	/**
	 * Menu Audits
	 */
	protected void testMenuAudits() throws Exception {
		trace("List for default query of [admin.Audit]");
		get("?a=l&m=admin&q=Audit");
	}

	/**
	 * Menu System Dashboard
	 */
	protected void testMenuSystemDashboard() throws Exception {
		trace("Edit new document [admin.SystemDashboard] instance");
		get("?a=e&m=admin&d=SystemDashboard");
	}

	/**
	 * Menu Document Numbers
	 */
	protected void testMenuDocumentNumbers() throws Exception {
		trace("List for default query of [admin.DocumentNumber]");
		get("?a=l&m=admin&q=DocumentNumber");
		trace("New row on list grid [admin.DocumentNumber] (s0)");
		listGridButton("s0", "s0:s6", false);
		trace("set moduleName (s7) if it exists and is not disabled");
		text("s7",
				"Sed ante est, rutrum vel diam id, vehicula euismod lorem. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Vestibulum vitae diam vel quam tempus suscipit vel at arcu. Etiam feugiat ipsum sed tellus vulputate congue.  Cras eget ligula diam. Suspendisse potenti. In id est at arcu aliquam venenatis. Integer pulvinar nunc dui, at blandit eros ultricies eget. Mauris sagittis. In sit amet mi eget libero varius pharetra sit amet quis dolor.");
		trace("set documentName (s14) if it exists and is not disabled");
		text("s14",
				"In sit amet mi eget libero varius pharetra sit amet quis dolor. Phasellus at vehicula tortor. Vestibulum cursus sollicitudin egestas.  Cras eget ligula diam. Quisque varius, sem eu varius molestie, eros augue cursus purus, nec mattis leo mauris vitae nisi. Aenean id orci sagittis odio vehicula mollis vel sed risus.");
		trace("set sequenceName (s21) if it exists and is not disabled");
		text("s21",
				"Vivamus vel neque pellentesque purus hendrerit finibus quis in magna. Curabitur aliquam accumsan purus, sed ultricies elit pulvinar in. Quisque lacus purus, suscipit et elit eget, interdum semper nibh. Duis nec tincidunt velit, id vestibulum sem. Suspendisse ut mi felis. Cras venenatis sit amet urna a eleifend. Curabitur eget orci a mi consequat sodales.");
		trace("set documentNumber (s28) if it exists and is not disabled");
		text("s28",
				"In non pretium erat, at imperdiet neque. Duis nec tincidunt velit, id vestibulum sem. Vestibulum tincidunt placerat euismod. Duis eget quam vehicula, hendrerit leo ac, ullamcorper augue. Nulla finibus feugiat sapien, at tincidunt nisi pharetra vel. In cursus dignissim est ut varius. Phasellus dapibus mauris in mattis ultrices. In hac habitasse platea dictumst. Phasellus at vehicula tortor. Nulla aliquam bibendum massa ut tristique. Proin nec pharetra orci.");
		trace("click [save] (s35) if it exists and is not disabled");
		button("s35", true, false);
		trace("Test Success");
		verifySuccess();
		trace("set moduleName (s7) if it exists and is not disabled");
		text("s7",
				"Aliquam quis lorem in est dignissim semper in vitae mauris. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. In id est at arcu aliquam venenatis. Pellentesque pellentesque, lectus at mattis egestas, metus nunc rutrum ipsum, ut imperdiet mi felis sed urna. Curabitur aliquam accumsan purus, sed ultricies elit pulvinar in. Sed ante est, rutrum vel diam id, vehicula euismod lorem. Curabitur varius quis quam a placerat.");
		trace("set documentName (s14) if it exists and is not disabled");
		text("s14",
				"Ut nec bibendum elit, vitae malesuada massa.  Aliquam mattis posuere imperdiet. Duis eget quam vehicula, hendrerit leo ac, ullamcorper augue. Maecenas vestibulum lacus id tellus eleifend, vel pellentesque ante lobortis. In placerat interdum vulputate. Fusce posuere laoreet tempus. Cras venenatis sit amet urna a eleifend. In hac habitasse platea dictumst. Vivamus ac libero et diam consectetur gravida non vitae nisl.");
		trace("set sequenceName (s21) if it exists and is not disabled");
		text("s21",
				"In sit amet mi eget libero varius pharetra sit amet quis dolor. Curabitur eget orci a mi consequat sodales. In placerat interdum vulputate.  Nunc facilisis velit ut tempor vestibulum. Suspendisse potenti. Vestibulum tincidunt placerat euismod.");
		trace("set documentNumber (s28) if it exists and is not disabled");
		text("s28",
				"In hac habitasse platea dictumst. Vestibulum cursus sollicitudin egestas.  Nunc facilisis velit ut tempor vestibulum. Aenean ut odio hendrerit, consequat orci a, ultrices massa.Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque lacus purus, suscipit et elit eget, interdum semper nibh. Integer pharetra eget erat non ornare. Suspendisse ut mi felis.");
		trace("click [save] (s35) if it exists and is not disabled");
		button("s35", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [delete] (s36) if it exists and is not disabled");
		redirectButton("s36", true);
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
		trace("click tab [Schedule]");
		tab("s3:s37");
		trace("click tab [Actual]");
		tab("s3:s5");
		trace("click tab [Schedule]");
		tab("s3:s37");
		trace("click tab [Actual]");
		tab("s3:s5");
		trace("click tab [Schedule]");
		tab("s3:s37");
		trace("click [Refresh] (s47) if it exists and is not disabled");
		button("s47", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [DeleteCompletedJobs] (s48) if it exists and is not disabled");
		button("s48", true, true);
		trace("Test Success");
		verifySuccess();
	}

}
