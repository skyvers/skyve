package sail.admin;

import org.junit.After;
import org.junit.Before;
import org.openqa.selenium.By;

import util.sail.BrowserConfiguration;
import util.sail.Devices;
import util.sail.PrimeFacesTest;

public class AdminSail extends PrimeFacesTest {
	private String pathToChromeDriver = "/Users/mike/chromedriver";
//	private String pathToFirefoxDriver = "/usr/local/bin/geckodriver";
	
	@Before
	public void setup() throws Exception {
		setupChrome(new BrowserConfiguration().baseUrl("http://localhost:8080/skyve/").pathToDriver(pathToChromeDriver).userAgentString(Devices.ipad.userAgentString));
//		setupFirefox(new BrowserConfiguration().baseUrl("http://localhost:8080/skyve/").pathToDriver(pathToFirefoxDriver).userAgentString(Devices.ipad.userAgentString));
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
	protected void testMenuPassword() throws Exception {
		trace("Edit new document [admin.ChangePassword] instance");
		get("?a=e&m=admin&d=ChangePassword");
		trace("set oldPassword (s7password) if it exists and is not disabled");
		text("s7password", "Vestibulum vitae");
		trace("set newPassword (s14password) if it exists and is not disabled");
		text("s14password", "Vestibulum cursus sollicitudin egestas.");
		trace("set confirmPassword (s21password) if it exists and is not disabled");
		text("s21password", "Vivamus ac libero et dia");
		trace("click [MakePasswordChange] (s32) if it exists and is not disabled");
		button("s32", true, false);
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
		text("s32", "");
		trace("set groupMembershipList (s39) if it exists and is not disabled");
		text("s39", "Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Vivamus vel neque pellentesque purus hendrerit finibus quis in magna. Aliquam quis lorem in est dignissim semper in vitae mauris.");
		trace("set lastLogin (s46) if it exists and is not disabled");
		text("s46", "22-May-2018 17:49");
		trace("click tab [Jobs]");
		tab("s82:s84");
		trace("click tab [Subscriptions]");
		tab("s82:s95");
		trace("click tab [Jobs]");
		tab("s82:s84");
		trace("click tab [Subscriptions]");
		tab("s82:s95");
		trace("click [UpdateMyDetails] (s68) if it exists and is not disabled");
		button("s68", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Jobs]");
		tab("s82:s84");
		trace("click tab [Subscriptions]");
		tab("s82:s95");
	}

	/**
	 * Menu Contacts
	 */
	protected void testMenuContacts() throws Exception {
		trace("List for default query of [admin.Contact]");
		get("?a=l&m=admin&q=Contact");
		trace("New row on list grid [admin.Contact] (s0)");
		listGridButton("s0", "s0:s7", false);
		trace("set name (s9) if it exists and is not disabled");
		text("s9", "Aliquam mattis posuere imperdiet. Vestibulum tincidunt placerat euismod. In placerat interdum vulputate. Duis eget quam vehicula, hendrerit leo ac, ullamcorper augue.");
		trace("set contactType (s16) if it exists and is not disabled");
		selectOne("s16", 2);
		trace("set email1 (s24) if it exists and is not disabled");
		text("s24", "XDRRICZPZIHDFRSEGHAIVXWYXOZWTTMBVDFOCTVXJRUWXFQDMKOYRDHOWRDYKFDQFLCQBIWPAXFDXRVDHUOYIDLKPAVFAJFTAIXTREYZBTOZNYCAUCOYICSQESGNAYBERVDLHIPJDZUOWBXLRYLBKZRMHMCMPZGGZJLUGTQJZIMNOPMNISHGPRVUQDFUUAGGCNKDGMAHZCKIQVIZUNQBWQRHRVGZGUUEYDCOZJAYZJJFTLVEMEWGVHVVU@YSDQTSNSNPAXUQPRLMBNNIBRYEWQFTFKYKGHHAMVESJKVBFQGMQJPCMOYFTIJFPPXSJUYGMUIJXGPSVHMLGECAUKENWEJOEUNPJGYZRVJQDBTJLQMQKSYBNETAJOKSZUMMWKZQYDSNMFKKYKVUTXPKKXGMMZCYZEUPHIEPCZTUUZGMFNTHMPAMVZLVWCQJLOPODCLMFPKYNVSNTPBCVLCNIEGDFINENPHCYXXUHPJSPEVRJBILQALCD.BV");
		trace("set mobile (s31) if it exists and is not disabled");
		text("s31", "0493 826 469");
		trace("click [save] (s50) if it exists and is not disabled");
		button("s50", true, false);
		trace("Test Success");
		verifySuccess();
		trace("set name (s9) if it exists and is not disabled");
		text("s9", "Donec convallis vitae leo eu hendrerit. Curabitur aliquam accumsan purus, sed ultricies elit pulvinar in.  In non pretium erat, at imperdiet neque.");
		trace("set contactType (s16) if it exists and is not disabled");
		selectOne("s16", 2);
		trace("set email1 (s24) if it exists and is not disabled");
		text("s24", "DDSWQCWMAULWNHQDOGBTVDZHDQIFHQYCZCPSUAHUQSENLFZWPGHCCPUGVMNDPHNIFVFBFFIHEZNPHKWCPLAEYITFVKODRJHEYWWLVLDVRRNOLLROJIBOXNTUVWUTSRYVILXMIFIAXRLPPYOXIAWAJDRSFRAGRQBIJJNZPSJTRTEMVSHXNLSKAARJLEIRGNAKSPBCOHXELXZBCCFLJYAHRJTSQTZCKAJJCQPVZYVJBDRNPWVNKSVGLOTJO@OSQCCCFPLNQZJBTRDWKRWBMNWWYFZNZSSVYBWIDMKONIUVPFOMLWJHUCSNACKEVTHIRECMEEUCEFPJTVJOZGKMNWDFNLJWUGAJRWNHDLGPMKRLOQMKHYNGTMFDOYZOHLYVUSVHTVOIDINNFVPSRZNTQWGXZXUHVLMMPKPQGLWJRQDBDXLFFITIYHZVQACNNVSTSZPVVYWVUZNWVEZNBFNNDGAGMDKQYXUQDHIDTPKJOZCATQHDDRVQA.BL");
		trace("set mobile (s31) if it exists and is not disabled");
		text("s31", "0419 730 349");
		trace("click [save] (s50) if it exists and is not disabled");
		button("s50", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [delete] (s51) if it exists and is not disabled");
		redirectButton("s51", true);
		trace("Test Success");
		verifySuccess();
	}

	/**
	 * Menu Communications
	 */
	protected void testMenuCommunications() throws Exception {
		trace("List for default query of [admin.Communication]");
		get("?a=l&m=admin&q=Communication");
		trace("New row on list grid [admin.Communication] (s0)");
		listGridButton("s0", "s0:s5", false);
		trace("click tab [Manage]");
		tab("s3:s5");
		trace("set description (s3:s11) if it exists and is not disabled");
		text("s3:s11", "Cras eget ligula diam. Duis eget quam vehicula, hendrerit leo ac, ullamcorper augue. Vestibulum cursus sollicitudin egestas.");
		trace("set system (s3:s17) if it exists and is not disabled");
		checkbox("s3:s17", Boolean.FALSE);
		trace("set moduleName (s3:s24) if it exists and is not disabled");
		selectOne("s3:s24", 1);
		trace("set documentName (s3:s32) if it exists and is not disabled");
		selectOne("s3:s32", 0);
		trace("set tag (s3:s40) if it exists and is not disabled");
		selectOne("s3:s40", 0);
		trace("set results (s3:s57) if it exists and is not disabled");
		text("s3:s57", "Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Curabitur varius ante quis arcu dignissim, ac elementum nunc pharetra. Duis eget quam vehicula, hendrerit leo ac, ullamcorper augue. Donec convallis vitae leo eu hendrerit. Quisque varius, sem eu varius molestie, eros augue cursus purus, nec mattis leo mauris vitae nisi. Curabitur id pretium nulla. Vestibulum cursus sollicitudin egestas. Vestibulum tincidunt placerat euismod. In id est at arcu aliquam venenatis. Vivamus maximus tellus neque, nec vestibulum urna gravida et.  Nunc facilisis velit ut tempor vestibulum. Nulla finibus feugiat sapien, at tincidunt nisi pharetra vel. In sit amet mi eget libero varius pharetra sit amet quis dolor. Cras venenatis sit amet urna a eleifend. Suspendisse potenti. Vestibulum vitae diam vel quam tempus suscipit vel at arcu. In id nisl elit. Vivamus vel neque pellentesque purus hendrerit finibus quis in magna. In cursus dignissim est ut varius. Duis nec tincidunt velit, id vestibulum sem.  In non pretium erat, at imperdiet neque.  Aliquam mattis posuere imperdiet.Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam dignissim ac libero vitae aliquam. In hac habitasse platea dictumst. Integer pulvinar nunc dui, at blandit eros ultricies eget.");
		trace("click tab [Contents]");
		tab("s3:s84");
		trace("set formatType (s3:s90) if it exists and is not disabled");
		selectOne("s3:s90", 1);
		trace("set sendFrom (s3:s102) if it exists and is not disabled");
		text("s3:s102", "Pellentesque pellentesque, lectus at mattis egestas, metus nunc rutrum ipsum, ut imperdiet mi felis sed urna.");
		trace("set monitorBcc (s3:s108) if it exists and is not disabled");
		checkbox("s3:s108", Boolean.FALSE);
		trace("set sendTo (s3:s124) if it exists and is not disabled");
		text("s3:s124", "Aliquam quis lorem in est dignissim semper in vitae mauris.");
		trace("set ccTo (s3:s131) if it exists and is not disabled");
		text("s3:s131", "Pellentesque pellentesque, lectus at mattis egestas, metus nunc rutrum ipsum, ut imperdiet mi felis sed urna. Vivamus vel neque pellentesque purus hendrerit finibus quis in magna. Sed in ultrices turpis.");
		trace("set subject (s3:s147) if it exists and is not disabled");
		text("s3:s147", "Phasellus facilisis, nibh eu tincidunt porttitor, magna diam aliquam magna, ut tincidunt erat magna pharetra est.");
		trace("set includeCalendar (s3:s173) if it exists and is not disabled");
		checkbox("s3:s173", Boolean.FALSE);
		trace("click tab [Options]");
		tab("s3:s187");
		trace("set attachmentFileName1 (s3:s205) if it exists and is not disabled");
		text("s3:s205", "Nunc facilisis velit ut tempor vestibulum. Ut nec bibendum elit, vitae malesuada massa.");
		trace("set attachmentFileName2 (s3:s224) if it exists and is not disabled");
		text("s3:s224", "Curabitur ultrices fermentum lectus a luctus. Suspendisse varius sit amet lorem vitae efficitur. Vestibulum et ex et ligula tincidunt pharetra. Mauris posuere erat at velit convallis, ut sodales lacus tristique.");
		trace("set attachmentFileName3 (s3:s243) if it exists and is not disabled");
		text("s3:s243", "Aenean ut odio hendrerit, consequat orci a, ultrices massa. Donec convallis vitae leo eu hendrerit.");
		trace("set calendarTitleExpression (s3:s253) if it exists and is not disabled");
		text("s3:s253", "In non pretium erat, at imperdiet neque. Curabitur varius ante quis arcu dignissim, ac elementum nunc pharetra.");
		trace("set calendarStartTime (s3:s260) if it exists and is not disabled");
		_input("s3:s260", "");
		trace("set calendarEndTime (s3:s267) if it exists and is not disabled");
		_input("s3:s267", "");
		trace("set calendarDescriptionExpression (s3:s274) if it exists and is not disabled");
		text("s3:s274", "Nulla finibus feugiat sapien, at tincidunt nisi pharetra vel. Vestibulum tincidunt placerat euismod. Suspendisse varius sit amet lorem vitae efficitur. Sed in nulla id nunc consectetur elementum ut non magna. Donec sed feugiat neque. Cras venenatis sit amet urna a eleifend.Lorem ipsum dolor sit amet, consectetur adipiscing elit. In hac habitasse platea dictumst.");
		trace("click tab [Subscriptions]");
		tab("s3:s280");
		trace("click tab [Manage]");
		tab("s3:s5");
		trace("click tab [Contents]");
		tab("s3:s84");
		trace("click tab [Options]");
		tab("s3:s187");
		trace("click tab [Subscriptions]");
		tab("s3:s280");
		trace("click [save] (s293) if it exists and is not disabled");
		button("s293", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Manage]");
		tab("s3:s5");
		trace("set description (s3:s11) if it exists and is not disabled");
		text("s3:s11", "Curabitur varius quis quam a placerat. Sed in ultrices turpis.  Cras eget ligula diam. Curabitur id pretium nulla.");
		trace("set system (s3:s17) if it exists and is not disabled");
		checkbox("s3:s17", Boolean.FALSE);
		trace("set moduleName (s3:s24) if it exists and is not disabled");
		selectOne("s3:s24", 3);
		trace("set documentName (s3:s32) if it exists and is not disabled");
		selectOne("s3:s32", 0);
		trace("set tag (s3:s40) if it exists and is not disabled");
		selectOne("s3:s40", 0);
		trace("set results (s3:s57) if it exists and is not disabled");
		text("s3:s57", "Vestibulum et ex et ligula tincidunt pharetra. Mauris posuere erat at velit convallis, ut sodales lacus tristique. Aenean id orci sagittis odio vehicula mollis vel sed risus. Nullam ullamcorper leo laoreet suscipit scelerisque. In cursus dignissim est ut varius. Phasellus at vehicula tortor. Duis nec tincidunt velit, id vestibulum sem. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. In placerat interdum vulputate.  Cras eget ligula diam.  Nunc facilisis velit ut tempor vestibulum. Donec sed feugiat neque. Vestibulum vitae diam vel quam tempus suscipit vel at arcu.Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer pulvinar nunc dui, at blandit eros ultricies eget. Proin nec pharetra orci. Praesent ut dignissim ligula. Curabitur ultrices fermentum lectus a luctus. Sed quis odio quam. Curabitur varius ante quis arcu dignissim, ac elementum nunc pharetra. Vivamus congue libero risus, nec cursus tortor congue eget. Maecenas vestibulum lacus id tellus eleifend, vel pellentesque ante lobortis. Ut nec bibendum elit, vitae malesuada massa. Donec eget elit neque. Pellentesque pellentesque, lectus at mattis egestas, metus nunc rutrum ipsum, ut imperdiet mi felis sed urna. Curabitur eget orci a mi consequat sodales. Curabitur varius quis quam a placerat. Suspendisse varius sit amet lorem vitae efficitur. Donec dui nibh, vehicula id scelerisque et, convallis eget nibh. Vivamus vel neque pellentesque purus hendrerit finibus quis in magna. Integer pharetra eget erat non ornare. In sit amet mi eget libero varius pharetra sit amet quis dolor.");
		trace("click tab [Contents]");
		tab("s3:s84");
		trace("set formatType (s3:s90) if it exists and is not disabled");
		selectOne("s3:s90", 1);
		trace("set sendFrom (s3:s102) if it exists and is not disabled");
		text("s3:s102", "Vestibulum tincidunt placerat euismod. Donec dui nibh, vehicula id scelerisque et, convallis eget nibh. Cras venenatis sit amet urna a eleifend.  Etiam luctus cursus fermentum.");
		trace("set monitorBcc (s3:s108) if it exists and is not disabled");
		checkbox("s3:s108", Boolean.FALSE);
		trace("set sendTo (s3:s124) if it exists and is not disabled");
		text("s3:s124", "In cursus dignissim est ut varius. Quisque lacus purus, suscipit et elit eget, interdum semper nibh. Curabitur eget orci a mi consequat sodales. Suspendisse ut mi felis.");
		trace("set ccTo (s3:s131) if it exists and is not disabled");
		text("s3:s131", "Phasellus dapibus mauris in mattis ultrices. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos.");
		trace("set subject (s3:s147) if it exists and is not disabled");
		text("s3:s147", "Quisque lacus purus, suscipit et elit eget, interdum semper nibh. Curabitur varius quis quam a placerat. Cras venenatis sit amet urna a eleifend. Nullam ullamcorper leo laoreet suscipit scelerisque.");
		trace("set includeCalendar (s3:s173) if it exists and is not disabled");
		checkbox("s3:s173", Boolean.FALSE);
		trace("click tab [Options]");
		tab("s3:s187");
		trace("set attachmentFileName1 (s3:s205) if it exists and is not disabled");
		text("s3:s205", "Curabitur aliquam accumsan purus, sed ultricies elit pulvinar in. Donec dui nibh, vehicula id scelerisque et, convallis eget nibh.");
		trace("set attachmentFileName2 (s3:s224) if it exists and is not disabled");
		text("s3:s224", "Aliquam mattis posuere imperdiet. Nullam dignissim ac libero vitae aliquam.");
		trace("set attachmentFileName3 (s3:s243) if it exists and is not disabled");
		text("s3:s243", "Vivamus porttitor, metus vitae lobortis mattis, libero metus tincidunt velit, nec dapibus ipsum arcu vel odio.");
		trace("set calendarTitleExpression (s3:s253) if it exists and is not disabled");
		text("s3:s253", "Quisque varius, sem eu varius molestie, eros augue cursus purus, nec mattis leo mauris vitae nisi. Mauris posuere erat at velit convallis, ut sodales lacus tristique. Mauris sagittis.");
		trace("set calendarStartTime (s3:s260) if it exists and is not disabled");
		_input("s3:s260", "");
		trace("set calendarEndTime (s3:s267) if it exists and is not disabled");
		_input("s3:s267", "");
		trace("set calendarDescriptionExpression (s3:s274) if it exists and is not disabled");
		text("s3:s274", "In non pretium erat, at imperdiet neque. Vestibulum cursus sollicitudin egestas. In placerat interdum vulputate. Vestibulum tincidunt placerat euismod. Nulla aliquam bibendum massa ut tristique.Lorem ipsum dolor sit amet, consectetur adipiscing elit.");
		trace("click tab [Subscriptions]");
		tab("s3:s280");
		trace("click [save] (s293) if it exists and is not disabled");
		button("s293", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Manage]");
		tab("s3:s5");
		trace("click [GetCount] (s3:s47) if it exists and is not disabled");
		button("s3:s47", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [GetResults] (s3:s65) if it exists and is not disabled");
		button("s3:s65", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [CreateFiles] (s3:s67) if it exists and is not disabled");
		button("s3:s67", true, true);
		trace("Test Success");
		verifySuccess();
		trace("click [SendNow] (s3:s69) if it exists and is not disabled");
		button("s3:s69", true, true);
		trace("Test Success");
		verifySuccess();
		trace("click [ZipBatch] (s3:s80) if it exists and is not disabled");
		button("s3:s80", true, true);
		trace("Test Success");
		verifySuccess();
		trace("click [DeleteBatch] (s3:s82) if it exists and is not disabled");
		button("s3:s82", true, true);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Contents]");
		tab("s3:s84");
		trace("click [TestSend] (s3:s97) if it exists and is not disabled");
		button("s3:s97", true, true);
		trace("Test Success");
		verifySuccess();
		trace("click [AddUnsubscribeLink] (s3:s179) if it exists and is not disabled");
		button("s3:s179", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [AddImage] (s3:s183) if it exists and is not disabled");
		button("s3:s183", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Options]");
		tab("s3:s187");
		trace("click tab [Subscriptions]");
		tab("s3:s280");
		trace("click [delete] (s294) if it exists and is not disabled");
		redirectButton("s294", true);
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
		listGridButton("s0", "s0:s3", false);
		trace("set name (s11) if it exists and is not disabled");
		text("s11", "Praesent ut digni");
		trace("set description (s18) if it exists and is not disabled");
		text("s18", "Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos.");
		trace("New row on data grid [roles] (s24)");
		dataGridButton("s24", "s24:s33", false);
		trace("set roleName (s7) if it exists and is not disabled");
		selectOne("s7", 14);
		trace("click [zoom out] (s17) if it exists and is not disabled");
		button("s17", false, false);
		trace("Test Success");
		verifySuccess();
		trace("click [save] (s45) if it exists and is not disabled");
		button("s45", true, false);
		trace("Test Success");
		verifySuccess();
		trace("set name (s11) if it exists and is not disabled");
		text("s11", "Curabitur varius an");
		trace("set description (s18) if it exists and is not disabled");
		text("s18", "Nulla aliquam bibendum massa ut tristique. Suspendisse ut mi felis. Curabitur id pretium nulla.");
		trace("click [save] (s45) if it exists and is not disabled");
		button("s45", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [delete] (s46) if it exists and is not disabled");
		redirectButton("s46", true);
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
		listGridButton("s0", "s0:s3", false);
		trace("set name (s7) if it exists and is not disabled");
		text("s7", "Donec convallis vitae");
		trace("set description (s14) if it exists and is not disabled");
		text("s14", "In id est at arcu aliquam venenatis. Donec convallis vitae leo eu hendrerit. In id nisl elit.  In non pretium erat, at imperdiet neque. Phasellus facilisis, nibh eu tincidunt porttitor, magna diam aliquam magna, ut tincidunt erat magna pharetra est. Curabitur varius ante quis arcu dignissim, ac elementum nunc pharetra. Phasellus dapibus mauris in mattis ultrices.  Etiam luctus cursus fermentum. Quisque varius, sem eu varius molestie, eros augue cursus purus, nec mattis leo mauris vitae nisi. Aliquam quis lorem in est dignissim semper in vitae mauris. Vivamus vel neque pellentesque purus hendrerit finibus quis in magna. Curabitur varius quis quam a placerat. Sed in ultrices turpis. Duis eget quam vehicula, hendrerit leo ac, ullamcorper augue. Integer pulvinar nunc dui, at blandit eros ultricies eget. Pellentesque pellentesque, lectus at mattis egestas, metus nunc rutrum ipsum, ut imperdiet mi felis sed urna. In placerat interdum vulputate. Sed ante est, rutrum vel diam id, vehicula euismod lorem. Etiam feugiat ipsum sed tellus vulputate congue. Integer pharetra eget erat non ornare. Phasellus at vehicula tortor. Curabitur id pretium nulla. Maecenas vestibulum lacus id tellus eleifend, vel pellentesque ante lobortis.");
		trace("click [save] (s21) if it exists and is not disabled");
		button("s21", true, false);
		trace("Test Success");
		verifySuccess();
		trace("set name (s7) if it exists and is not disabled");
		text("s7", "Curabitur vari");
		trace("set description (s14) if it exists and is not disabled");
		text("s14", "In id nisl elit. Sed quis odio quam. Fusce posuere laoreet tempus. Donec convallis vitae leo eu hendrerit. Nulla finibus feugiat sapien, at tincidunt nisi pharetra vel. Curabitur ultrices fermentum lectus a luctus. Phasellus dapibus mauris in mattis ultrices. Nulla aliquam bibendum massa ut tristique. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Sed in nulla id nunc consectetur elementum ut non magna. Suspendisse ut mi felis. Vestibulum et ex et ligula tincidunt pharetra. Vivamus porttitor, metus vitae lobortis mattis, libero metus tincidunt velit, nec dapibus ipsum arcu vel odio. Curabitur varius ante quis arcu dignissim, ac elementum nunc pharetra. Curabitur id pretium nulla. Duis nec tincidunt velit, id vestibulum sem.");
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
	 * Menu DevOps::Data Maintenance
	 */
	protected void testMenuDevOpsDataMaintenance() throws Exception {
		trace("Edit new document [admin.DataMaintenance] instance");
		get("?a=e&m=admin&d=DataMaintenance");
		trace("click tab [Export/Import]");
		tab("s3:s5");
		trace("set modDocName (s3:s11) if it exists and is not disabled");
		selectOne("s3:s11", 28);
		trace("click tab [Backup/Restore]");
		tab("s3:s36");
		trace("set dailyBackupRetention (s3:s42) if it exists and is not disabled");
		_input("s3:s42", "5297");
		trace("set weeklyBackupRetention (s3:s48) if it exists and is not disabled");
		_input("s3:s48", "6870");
		trace("set monthlyBackupRetention (s3:s54) if it exists and is not disabled");
		_input("s3:s54", "7222");
		trace("set yearlyBackupRetention (s3:s60) if it exists and is not disabled");
		_input("s3:s60", "7381");
		trace("set restorePreProcess (s3:s78) if it exists and is not disabled");
		selectOne("s3:s78", 2);
		trace("click tab [Data]");
		tab("s3:s109");
		trace("set schemaName (s3:s115) if it exists and is not disabled");
		text("s3:s115", "Sed in nulla id nunc consectetur elementum ut non magna. Ut nec bibendum elit, vitae malesuada massa. Sed ante est, rutrum vel diam id, vehicula euismod lorem. Curabitur aliquam accumsan purus, sed ultricies elit pulvinar in. Donec sed feugiat neque. Integer pharetra diam eget felis imperdiet, ut tempus libero porttitor. Curabitur eget orci a mi consequat sodales. Nullam ullamcorper leo laoreet suscipit scelerisque. In id est at arcu aliquam venenatis.  In non pretium erat, at imperdiet neque. In id nisl elit. Maecenas vestibulum lacus id tellus eleifend, vel pellentesque ante lobortis.  Cras eget ligula diam.");
		trace("click tab [Content]");
		tab("s3:s144");
		trace("click tab [Manage Audits]");
		tab("s3:s172");
		trace("set auditModuleName (s3:s178) if it exists and is not disabled");
		selectOne("s3:s178", 2);
		trace("set auditDocumentName (s3:s186) if it exists and is not disabled");
		selectOne("s3:s186", 2);
		trace("set auditOperation (s3:s194) if it exists and is not disabled");
		selectOne("s3:s194", 1);
		trace("set auditTimestampStart (s3:s202) if it exists and is not disabled");
		_input("s3:s202", "22-May-2018 17:49:19");
		trace("set auditTimestampEnd (s3:s209) if it exists and is not disabled");
		_input("s3:s209", "22-May-2018 17:49:19");
		trace("set auditMatchCount (s3:s223) if it exists and is not disabled");
		text("s3:s223", "4555");
		trace("click tab [Refresh]");
		tab("s3:s265");
		trace("set refreshOption (s3:s286) if it exists and is not disabled");
		selectOne("s3:s286", 2);
		trace("set evictOption (s3:s293) if it exists and is not disabled");
		selectOne("s3:s293", 1);
		trace("set notification (s3:s300) if it exists and is not disabled");
		checkbox("s3:s300", Boolean.FALSE);
		trace("click tab [Export/Import]");
		tab("s3:s5");
		trace("click tab [Backup/Restore]");
		tab("s3:s36");
		trace("click tab [Data]");
		tab("s3:s109");
		trace("click tab [Content]");
		tab("s3:s144");
		trace("click tab [Manage Audits]");
		tab("s3:s172");
		trace("click tab [Refresh]");
		tab("s3:s265");
		trace("click [save] (s311) if it exists and is not disabled");
		button("s311", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Export/Import]");
		tab("s3:s5");
		trace("set modDocName (s3:s11) if it exists and is not disabled");
		selectOne("s3:s11", 6);
		trace("click tab [Backup/Restore]");
		tab("s3:s36");
		trace("set dailyBackupRetention (s3:s42) if it exists and is not disabled");
		_input("s3:s42", "669");
		trace("set weeklyBackupRetention (s3:s48) if it exists and is not disabled");
		_input("s3:s48", "1473");
		trace("set monthlyBackupRetention (s3:s54) if it exists and is not disabled");
		_input("s3:s54", "8871");
		trace("set yearlyBackupRetention (s3:s60) if it exists and is not disabled");
		_input("s3:s60", "402");
		trace("set restorePreProcess (s3:s78) if it exists and is not disabled");
		selectOne("s3:s78", 1);
		trace("click tab [Data]");
		tab("s3:s109");
		trace("set schemaName (s3:s115) if it exists and is not disabled");
		text("s3:s115", "Suspendisse ut mi felis. Praesent ut dignissim ligula.  Aliquam mattis posuere imperdiet. Nulla aliquam bibendum massa ut tristique. Duis nec tincidunt velit, id vestibulum sem. Vestibulum vitae diam vel quam tempus suscipit vel at arcu. Curabitur aliquam accumsan purus, sed ultricies elit pulvinar in.Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer pulvinar nunc dui, at blandit eros ultricies eget.");
		trace("click tab [Content]");
		tab("s3:s144");
		trace("click tab [Manage Audits]");
		tab("s3:s172");
		trace("set auditModuleName (s3:s178) if it exists and is not disabled");
		selectOne("s3:s178", 3);
		trace("set auditDocumentName (s3:s186) if it exists and is not disabled");
		selectOne("s3:s186", 1);
		trace("set auditOperation (s3:s194) if it exists and is not disabled");
		selectOne("s3:s194", 3);
		trace("set auditTimestampStart (s3:s202) if it exists and is not disabled");
		_input("s3:s202", "22-May-2018 17:49:19");
		trace("set auditTimestampEnd (s3:s209) if it exists and is not disabled");
		_input("s3:s209", "22-May-2018 17:49:19");
		trace("set auditMatchCount (s3:s223) if it exists and is not disabled");
		text("s3:s223", "2666");
		trace("click tab [Refresh]");
		tab("s3:s265");
		trace("set refreshOption (s3:s286) if it exists and is not disabled");
		selectOne("s3:s286", 1);
		trace("set evictOption (s3:s293) if it exists and is not disabled");
		selectOne("s3:s293", 2);
		trace("set notification (s3:s300) if it exists and is not disabled");
		checkbox("s3:s300", Boolean.FALSE);
		trace("click [save] (s311) if it exists and is not disabled");
		button("s311", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Export/Import]");
		tab("s3:s5");
		trace("click [DataMaintenanceExportAction] (s3:s23) if it exists and is not disabled");
		button("s3:s23", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [DataMaintenanceImportAction] (s3:s32) if it exists and is not disabled");
		button("s3:s32", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Backup/Restore]");
		tab("s3:s36");
		trace("click [Restore] (s3:s85) if it exists and is not disabled");
		button("s3:s85", true, true);
		trace("Test Success");
		verifySuccess();
		trace("click [Backup] (s3:s100) if it exists and is not disabled");
		button("s3:s100", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [DownloadBackup] (s3:s103) if it exists and is not disabled");
		button("s3:s103", true, true);
		trace("Test Success");
		verifySuccess();
		trace("click [UploadBackup] (s3:s105) if it exists and is not disabled");
		button("s3:s105", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [DeleteBackup] (s3:s107) if it exists and is not disabled");
		button("s3:s107", true, true);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Data]");
		tab("s3:s109");
		trace("click [Truncate] (s3:s121) if it exists and is not disabled");
		button("s3:s121", true, true);
		trace("Test Success");
		verifySuccess();
		trace("click [Create] (s3:s138) if it exists and is not disabled");
		button("s3:s138", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [Drop] (s3:s140) if it exists and is not disabled");
		button("s3:s140", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [Sync] (s3:s142) if it exists and is not disabled");
		button("s3:s142", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Content]");
		tab("s3:s144");
		trace("click [CheckContent] (s3:s166) if it exists and is not disabled");
		button("s3:s166", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [RelinkContent] (s3:s168) if it exists and is not disabled");
		button("s3:s168", true, true);
		trace("Test Success");
		verifySuccess();
		trace("click [Reindex] (s3:s170) if it exists and is not disabled");
		button("s3:s170", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Manage Audits]");
		tab("s3:s172");
		trace("click [CheckAuditMatches] (s3:s229) if it exists and is not disabled");
		button("s3:s229", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [TruncateAuditLog] (s3:s241) if it exists and is not disabled");
		button("s3:s241", true, true);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Refresh]");
		tab("s3:s265");
		trace("click [RefreshDocumentTuples] (s3:s306) if it exists and is not disabled");
		button("s3:s306", true, true);
		trace("Test Success");
		verifySuccess();
		trace("click [save] (s311) if it exists and is not disabled");
		button("s311", true, false);
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
		text("s3:s11", "Aliquam mattis posuere imperdiet. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Mauris sagittis. Duis eget quam vehicula, hendrerit leo ac, ullamcorper augue. Vivamus porttitor, metus vitae lobortis mattis, libero metus tincidunt velit, nec dapibus ipsum arcu vel odio. Curabitur varius ante quis arcu dignissim, ac elementum nunc pharetra. Proin nec pharetra orci. Nullam ullamcorper leo laoreet suscipit scelerisque. Curabitur varius quis quam a placerat. Curabitur id pretium nulla.  In non pretium erat, at imperdiet neque. Integer pulvinar nunc dui, at blandit eros ultricies eget. Suspendisse ut mi felis. Nulla aliquam bibendum massa ut tristique. Vivamus ac libero et diam consectetur gravida non vitae nisl. Cras venenatis sit amet urna a eleifend. Quisque varius, sem eu varius molestie, eros augue cursus purus, nec mattis leo mauris vitae nisi. Vivamus congue libero risus, nec cursus tortor congue eget. Duis nec tincidunt velit, id vestibulum sem. Donec convallis vitae leo eu hendrerit. In placerat interdum vulputate. Phasellus at vehicula tortor. Quisque lacus purus, suscipit et elit eget, interdum semper nibh. Vivamus maximus tellus neque, nec vestibulum urna gravida et.  Cras eget ligula diam. Sed ante est, rutrum vel diam id, vehicula euismod lorem. Vestibulum et ex et ligula tincidunt pharetra. Suspendisse potenti. Curabitur eget orci a mi consequat sodales. Aenean ut odio hendrerit, consequat orci a, ultrices massa. Nulla risus libero, pharetra ut nisi luctus, commodo vestibulum velit. Integer pharetra eget erat non ornare. In sit amet mi eget libero varius pharetra sit amet quis dolor. In id nisl elit. Sed in ultrices turpis. Vestibulum vitae diam vel quam tempus suscipit vel at arcu. Phasellus facilisis, nibh eu tincidunt porttitor, magna diam aliquam magna, ut tincidunt erat magna pharetra est.");
		trace("set outputLocation (s3:s18) if it exists and is not disabled");
		text("s3:s18", "Praesent ut dignissim ligula. Sed in nulla id nunc consectetur elementum ut non magna. Nullam ullamcorper leo laoreet suscipit scelerisque.");
		trace("set defaultModule (s3:s25) if it exists and is not disabled");
		selectOne("s3:s25", 3);
		trace("click tab [Markdown Preview]");
		tab("s3:s32");
		trace("click tab [Document Preview]");
		tab("s3:s49");
		trace("click tab [Help]");
		tab("s3:s58");
		trace("click tab [Input]");
		tab("s3:s5");
		trace("click tab [Markdown Preview]");
		tab("s3:s32");
		trace("click tab [Document Preview]");
		tab("s3:s49");
		trace("click tab [Help]");
		tab("s3:s58");
		trace("click tab [Input]");
		tab("s3:s5");
		trace("click tab [Markdown Preview]");
		tab("s3:s32");
		trace("click tab [Document Preview]");
		tab("s3:s49");
		trace("click tab [Help]");
		tab("s3:s58");
		trace("click [Submit] (s68) if it exists and is not disabled");
		button("s68", true, true);
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
		listGridButton("s0", "s0:s4", false);
		trace("set moduleName (s7) if it exists and is not disabled");
		text("s7", "Curabitur varius quis quam a placerat.");
		trace("set queryName (s14) if it exists and is not disabled");
		text("s14", "Curabitur aliquam accumsan purus, sed ultri");
		trace("set name (s21) if it exists and is not disabled");
		text("s21", "Duis eget quam vehicula, hendrerit leo ac, ullamcorper augue. Sed ante est, rutrum vel diam id, vehicula euismod lorem. Vivamus congue libero risus, nec cursus tortor congue eget.");
		trace("set snapshot (s28) if it exists and is not disabled");
		text("s28", "Suspendisse ut mi felis. Curabitur varius quis quam a placerat. In id nisl elit.  Cras eget ligula diam. In cursus dignissim est ut varius. Nulla aliquam bibendum massa ut tristique. Quisque lacus purus, suscipit et elit eget, interdum semper nibh. Vestibulum tincidunt placerat euismod. Integer pharetra diam eget felis imperdiet, ut tempus libero porttitor. Curabitur ultrices fermentum lectus a luctus. Vestibulum cursus sollicitudin egestas. Aliquam quis lorem in est dignissim semper in vitae mauris. Suspendisse varius sit amet lorem vitae efficitur.  Nunc facilisis velit ut tempor vestibulum. Phasellus facilisis, nibh eu tincidunt porttitor, magna diam aliquam magna, ut tincidunt erat magna pharetra est. Donec sed feugiat neque. In sit amet mi eget libero varius pharetra sit amet quis dolor. Sed quis odio quam. In id est at arcu aliquam venenatis. Sed ante est, rutrum vel diam id, vehicula euismod lorem.  Etiam luctus cursus fermentum. Curabitur id pretium nulla. Phasellus dapibus mauris in mattis ultrices. Nulla finibus feugiat sapien, at tincidunt nisi pharetra vel. Fusce posuere laoreet tempus. Pellentesque pellentesque, lectus at mattis egestas, metus nunc rutrum ipsum, ut imperdiet mi felis sed urna. Nullam ullamcorper leo laoreet suscipit scelerisque. Vivamus ac libero et diam consectetur gravida non vitae nisl. Sed in nulla id nunc consectetur elementum ut non magna. Donec eget elit neque. Quisque varius, sem eu varius molestie, eros augue cursus purus, nec mattis leo mauris vitae nisi. Curabitur eget orci a mi consequat sodales. Vivamus congue libero risus, nec cursus tortor congue eget. Integer pharetra eget erat non ornare.");
		trace("set copyToUserSnapshotName (s45) if it exists and is not disabled");
		text("s45", "Donec eget elit neque. In id est at arcu aliquam venenatis. Sed in ultrices turpis.");
		trace("click [save] (s61) if it exists and is not disabled");
		button("s61", true, false);
		trace("Test Success");
		verifySuccess();
		trace("set moduleName (s7) if it exists and is not disabled");
		text("s7", "Donec convallis vitae leo eu hendrerit.");
		trace("set queryName (s14) if it exists and is not disabled");
		text("s14", "Duis eget quam vehicula, hendrerit leo ac, ullamcorper augue.");
		trace("set name (s21) if it exists and is not disabled");
		text("s21", "Curabitur varius ante quis arcu dignissim, ac elementum nunc pharetra. Vivamus porttitor, metus vitae lobortis mattis, libero metus tincidunt velit, nec dapibus ipsum arcu vel odio. Curabitur id pretium nulla.");
		trace("set snapshot (s28) if it exists and is not disabled");
		text("s28", "Vivamus ac libero et diam consectetur gravida non vitae nisl. In id est at arcu aliquam venenatis. Vestibulum tincidunt placerat euismod. Phasellus facilisis, nibh eu tincidunt porttitor, magna diam aliquam magna, ut tincidunt erat magna pharetra est.  In non pretium erat, at imperdiet neque. Integer pharetra diam eget felis imperdiet, ut tempus libero porttitor. Maecenas vestibulum lacus id tellus eleifend, vel pellentesque ante lobortis. In placerat interdum vulputate. Suspendisse ut mi felis. Curabitur id pretium nulla.  Aliquam mattis posuere imperdiet. Donec convallis vitae leo eu hendrerit. Vestibulum vitae diam vel quam tempus suscipit vel at arcu. Curabitur aliquam accumsan purus, sed ultricies elit pulvinar in. Fusce posuere laoreet tempus. Praesent ut dignissim ligula. Proin nec pharetra orci. Donec dui nibh, vehicula id scelerisque et, convallis eget nibh. Curabitur varius quis quam a placerat.  Nunc facilisis velit ut tempor vestibulum. In sit amet mi eget libero varius pharetra sit amet quis dolor. Cras venenatis sit amet urna a eleifend.");
		trace("set copyToUserSnapshotName (s45) if it exists and is not disabled");
		text("s45", "Vestibulum vitae diam vel quam tempus suscipit vel at arcu. In id nisl elit. Etiam feugiat ipsum sed tellus vulputate congue.");
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
		listGridButton("s0", "s0:s5", false);
		trace("set moduleName (s7) if it exists and is not disabled");
		text("s7", "Sed in nulla id nunc consectetur elementum ut non magna. Praesent ut dignissim ligula. In id est at arcu aliquam venenatis. Suspendisse potenti. Aliquam quis lorem in est dignissim semper in vitae mauris. Fusce posuere laoreet tempus. Vestibulum vitae diam vel quam tempus suscipit vel at arcu. Vestibulum cursus sollicitudin egestas.");
		trace("set documentName (s14) if it exists and is not disabled");
		text("s14", "Vestibulum tincidunt placerat euismod. In hac habitasse platea dictumst. Ut nec bibendum elit, vitae malesuada massa. Aliquam quis lorem in est dignissim semper in vitae mauris. Aenean ut odio hendrerit, consequat orci a, ultrices massa.Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vestibulum vitae diam vel quam tempus suscipit vel at arcu. In id est at arcu aliquam venenatis. Proin nec pharetra orci. Sed quis odio quam. Mauris sagittis.");
		trace("set sequenceName (s21) if it exists and is not disabled");
		text("s21", "Quisque varius, sem eu varius molestie, eros augue cursus purus, nec mattis leo mauris vitae nisi. In placerat interdum vulputate. Etiam feugiat ipsum sed tellus vulputate congue.");
		trace("set documentNumber (s28) if it exists and is not disabled");
		text("s28", "Maecenas vestibulum lacus id tellus eleifend, vel pellentesque ante lobortis. Quisque varius, sem eu varius molestie, eros augue cursus purus, nec mattis leo mauris vitae nisi. Duis nec tincidunt velit, id vestibulum sem. Suspendisse ut mi felis.");
		trace("click [save] (s35) if it exists and is not disabled");
		button("s35", true, false);
		trace("Test Success");
		verifySuccess();
		trace("set moduleName (s7) if it exists and is not disabled");
		text("s7", "Vivamus porttitor, metus vitae lobortis mattis, libero metus tincidunt velit, nec dapibus ipsum arcu vel odio. Integer pulvinar nunc dui, at blandit eros ultricies eget. Nulla risus libero, pharetra ut nisi luctus, commodo vestibulum velit. Vivamus congue libero risus, nec cursus tortor congue eget. Suspendisse ut mi felis. Integer pharetra eget erat non ornare. Quisque lacus purus, suscipit et elit eget, interdum semper nibh.");
		trace("set documentName (s14) if it exists and is not disabled");
		text("s14", "Phasellus facilisis, nibh eu tincidunt porttitor, magna diam aliquam magna, ut tincidunt erat magna pharetra est. Etiam feugiat ipsum sed tellus vulputate congue. Suspendisse potenti. Vivamus porttitor, metus vitae lobortis mattis, libero metus tincidunt velit, nec dapibus ipsum arcu vel odio. Pellentesque pellentesque, lectus at mattis egestas, metus nunc rutrum ipsum, ut imperdiet mi felis sed urna. Cras venenatis sit amet urna a eleifend. In hac habitasse platea dictumst.");
		trace("set sequenceName (s21) if it exists and is not disabled");
		text("s21", "In non pretium erat, at imperdiet neque. Integer pharetra eget erat non ornare. Maecenas vestibulum lacus id tellus eleifend, vel pellentesque ante lobortis.");
		trace("set documentNumber (s28) if it exists and is not disabled");
		text("s28", "Duis nec tincidunt velit, id vestibulum sem. Nulla finibus feugiat sapien, at tincidunt nisi pharetra vel. Phasellus facilisis, nibh eu tincidunt porttitor, magna diam aliquam magna, ut tincidunt erat magna pharetra est. Etiam feugiat ipsum sed tellus vulputate congue. Vestibulum cursus sollicitudin egestas. Curabitur id pretium nulla. Suspendisse varius sit amet lorem vitae efficitur.");
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
		tab("s3:s36");
		trace("click tab [Actual]");
		tab("s3:s5");
		trace("click tab [Schedule]");
		tab("s3:s36");
		trace("click tab [Actual]");
		tab("s3:s5");
		trace("click tab [Schedule]");
		tab("s3:s36");
		trace("click [Refresh] (s45) if it exists and is not disabled");
		button("s45", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [DeleteCompletedJobs] (s46) if it exists and is not disabled");
		button("s46", true, true);
		trace("Test Success");
		verifySuccess();
	}
}
