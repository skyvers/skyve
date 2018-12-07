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
		text("s7password", "Maecenas vestibul");
		trace("set newPassword (s14password) if it exists and is not disabled");
		text("s14password", "Vestibulum tincidunt placerat euismod");
		trace("set confirmPassword (s21password) if it exists and is not disabled");
		text("s21password", "In cursus dignissim e");
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
		text("s39", "Donec convallis vitae leo eu hendrerit. Vivamus ac libero et diam consectetur gravida non vitae nisl. Sed quis odio quam. Pellentesque pellentesque, lectus at mattis egestas, metus nunc rutrum ipsum, ut imperdiet mi felis sed urna. Duis eget quam vehicula, hendrerit leo ac, ullamcorper augue. In hac habitasse platea dictumst. Quisque varius, sem eu varius molestie, eros augue cursus purus, nec mattis leo mauris vitae nisi.");
		trace("set lastLogin (s46) if it exists and is not disabled");
		text("s46", "17-Oct-2018 17:21");
		trace("click tab [Jobs]");
		tab("s82:s84");
		trace("click tab [Subscriptions]");
		tab("s82:s96");
		trace("click tab [Jobs]");
		tab("s82:s84");
		trace("click tab [Subscriptions]");
		tab("s82:s96");
		trace("click [UpdateMyDetails] (s68) if it exists and is not disabled");
		button("s68", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Jobs]");
		tab("s82:s84");
		trace("click tab [Subscriptions]");
		tab("s82:s96");
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
		text("s9", "Mauris sagittis. Nulla risus libero, pharetra ut nisi luctus, commodo vestibulum velit.");
		trace("set contactType (s16) if it exists and is not disabled");
		selectOne("s16", 2);
		trace("set email1 (s24) if it exists and is not disabled");
		text("s24", "LWLEFECWSINZUDSCHUTISTRLIYXWITZBCAOFKTRRLZEMZSLFWKVVQQIGRVFZSUMLZTFAYFIQSZEBMJRAZUNBRGYFVSVPVVOMCNGDIENLFSESWWMYRUWLVIQWTWTJJQRAAOEFQGOUFMMKQJLNNBTAWNEFAWCZENVHYIJLALFZONDUTKXKXOZSYTGLYNNFCKYBXGOYJQUOELMMMTGWENXBNZYXANQYPWKDGVCPLGUQIQSDJZTGHNABKBNEO@AXVEUWEHUFBAQYWMFUFTZKYSTDHHYLMNORWTZGNIPTZMYFHLARSASHKJYJKCZNMTJJHGEFQVHUQKWHXKXTIQLTQEXUQCSPCIODBRGQKYJAMSLBWUJJSDAJQJVCUXPEUFFPXDQLADBXLUARQQIWBKMSLPTOKWJNTGXXLMETUDYJYBNMUZFTZGWRCWWHYQJUXDEYWSZAPIOPOTAQLWGAMHKUCWVUTFDSOYVZDDPVEYLFSUOPPCYXMUSBW.SK");
		trace("set mobile (s31) if it exists and is not disabled");
		text("s31", "0453 580 611");
		trace("click [save] (s50) if it exists and is not disabled");
		button("s50", true, false);
		trace("Test Success");
		verifySuccess();
		trace("set name (s9) if it exists and is not disabled");
		text("s9", "Ut nec bibendum elit, vitae malesuada massa. Aenean ut odio hendrerit, consequat orci a, ultrices massa.  Nunc facilisis velit ut tempor vestibulum.");
		trace("set contactType (s16) if it exists and is not disabled");
		selectOne("s16", 1);
		trace("set email1 (s24) if it exists and is not disabled");
		text("s24", "LRUJFRUQKPMRNYAKMUCCZKTDSFFNUWNIUJJDKIQIWQHMKBDCFFBUVNLUCDRLVKSOONGEZJMUIOQBANXPDGJRVCNLKXQEZHJIDTXPRGVKWUBFRQENPMWFCUNUIXFQVEOFLFEWXIGVHMQFIZUHHAXOYZYMVRFTMQLNVYNUYGHXHAQJRDTOKQUQDLLBHSXGZCIAFETDCMRVQCACJADACKMWDZYMCTRUPKTOSAUCDNQQUYJASKUJWKRTVQDNZ@UFCNFMNGGWHHUPVQQFNJQYPOTACJYLRGZQXYYAWQPXYZGDJPYINNOCMLUODIJVETFGLDZSCTACHGXZWVGKGDVMIQTOEDGEIIJGWWWJUYJRCGXPYWSCROIFWLKJHPVWAPMTIHBMAWIGUYORAHSCWJZGTPDWLUKLRSMDSRRJNLOAKEXCEWRQTJTDVAETQJARCPYQMRRZMBAJCWGDJVVDBZRMAKDJRSHXAKFAQWGNGZQXXODSYKPBMTODF.PU");
		trace("set mobile (s31) if it exists and is not disabled");
		text("s31", "0465 434 187");
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
		listGridButton("s0", "s0:s6", false);
		trace("click tab [Manage]");
		tab("s3:s5");
		trace("set description (s3:s11) if it exists and is not disabled");
		text("s3:s11", "Praesent ut dignissim ligula. Etiam feugiat ipsum sed tellus vulputate congue.");
		trace("set system (s3:s17) if it exists and is not disabled");
		checkbox("s3:s17", Boolean.FALSE);
		trace("set moduleName (s3:s24) if it exists and is not disabled");
		selectOne("s3:s24", 2);
		trace("set documentName (s3:s32) if it exists and is not disabled");
		selectOne("s3:s32", 0);
		trace("set tag (s3:s40) if it exists and is not disabled");
		selectOne("s3:s40", 0);
		trace("set results (s3:s57) if it exists and is not disabled");
		text("s3:s57", "Praesent ut dignissim ligula. Nullam ullamcorper leo laoreet suscipit scelerisque. Proin nec pharetra orci. Duis eget quam vehicula, hendrerit leo ac, ullamcorper augue. Maecenas vestibulum lacus id tellus eleifend, vel pellentesque ante lobortis. Suspendisse potenti. Mauris posuere erat at velit convallis, ut sodales lacus tristique. Donec dui nibh, vehicula id scelerisque et, convallis eget nibh. Curabitur varius ante quis arcu dignissim, ac elementum nunc pharetra.  In non pretium erat, at imperdiet neque. Duis nec tincidunt velit, id vestibulum sem. Quisque varius, sem eu varius molestie, eros augue cursus purus, nec mattis leo mauris vitae nisi.  Cras eget ligula diam. Fusce posuere laoreet tempus.");
		trace("click tab [Contents]");
		tab("s3:s84");
		trace("set formatType (s3:s90) if it exists and is not disabled");
		selectOne("s3:s90", 1);
		trace("set sendFrom (s3:s102) if it exists and is not disabled");
		text("s3:s102", "Phasellus facilisis, nibh eu tincidunt porttitor, magna diam aliquam magna, ut tincidunt erat magna pharetra est.");
		trace("set monitorBcc (s3:s108) if it exists and is not disabled");
		checkbox("s3:s108", Boolean.FALSE);
		trace("set sendTo (s3:s124) if it exists and is not disabled");
		text("s3:s124", "Pellentesque pellentesque, lectus at mattis egestas, metus nunc rutrum ipsum, ut imperdiet mi felis sed urna. Vivamus ac libero et diam consectetur gravida non vitae nisl.");
		trace("set ccTo (s3:s131) if it exists and is not disabled");
		text("s3:s131", "Vivamus porttitor, metus vitae lobortis mattis, libero metus tincidunt velit, nec dapibus ipsum arcu vel odio. Donec eget elit neque. Phasellus at vehicula tortor.");
		trace("set subject (s3:s147) if it exists and is not disabled");
		text("s3:s147", "Curabitur varius quis quam a placerat. Sed in ultrices turpis.");
		trace("set template (s3:s170) if it exists and is not disabled");
		selectOne("s3:s170", 0);
		trace("set includeCalendar (s3:s181) if it exists and is not disabled");
		checkbox("s3:s181", Boolean.FALSE);
		trace("click tab [Options]");
		tab("s3:s195");
		trace("set attachmentFileName1 (s3:s213) if it exists and is not disabled");
		text("s3:s213", "In cursus dignissim est ut varius. In hac habitasse platea dictumst.");
		trace("set attachmentFileName2 (s3:s232) if it exists and is not disabled");
		text("s3:s232", "Etiam feugiat ipsum sed tellus vulputate congue.  In non pretium erat, at imperdiet neque. In id est at arcu aliquam venenatis. Vestibulum tincidunt placerat euismod.  Etiam luctus cursus fermentum.");
		trace("set attachmentFileName3 (s3:s251) if it exists and is not disabled");
		text("s3:s251", "Donec convallis vitae leo eu hendrerit. Vivamus porttitor, metus vitae lobortis mattis, libero metus tincidunt velit, nec dapibus ipsum arcu vel odio.");
		trace("set calendarTitleExpression (s3:s261) if it exists and is not disabled");
		text("s3:s261", "In cursus dignissim est ut varius. Sed quis odio quam.");
		trace("set calendarStartTime (s3:s268) if it exists and is not disabled");
		_input("s3:s268", "");
		trace("set calendarEndTime (s3:s275) if it exists and is not disabled");
		_input("s3:s275", "");
		trace("set calendarDescriptionExpression (s3:s282) if it exists and is not disabled");
		text("s3:s282", "Vivamus ac libero et diam consectetur gravida non vitae nisl. In id nisl elit. In cursus dignissim est ut varius.");
		trace("click tab [Subscriptions]");
		tab("s3:s288");
		trace("click tab [Manage]");
		tab("s3:s5");
		trace("click tab [Contents]");
		tab("s3:s84");
		trace("click tab [Options]");
		tab("s3:s195");
		trace("click tab [Subscriptions]");
		tab("s3:s288");
		trace("click [save] (s302) if it exists and is not disabled");
		button("s302", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Manage]");
		tab("s3:s5");
		trace("set description (s3:s11) if it exists and is not disabled");
		text("s3:s11", "Vestibulum cursus sollicitudin egestas. Phasellus facilisis, nibh eu tincidunt porttitor, magna diam aliquam magna, ut tincidunt erat magna pharetra est. Proin nec pharetra orci.");
		trace("set system (s3:s17) if it exists and is not disabled");
		checkbox("s3:s17", Boolean.FALSE);
		trace("set moduleName (s3:s24) if it exists and is not disabled");
		selectOne("s3:s24", 2);
		trace("set documentName (s3:s32) if it exists and is not disabled");
		selectOne("s3:s32", 0);
		trace("set tag (s3:s40) if it exists and is not disabled");
		selectOne("s3:s40", 0);
		trace("set results (s3:s57) if it exists and is not disabled");
		text("s3:s57", "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec sed feugiat neque. Donec convallis vitae leo eu hendrerit. Phasellus dapibus mauris in mattis ultrices. Duis eget quam vehicula, hendrerit leo ac, ullamcorper augue. Nulla aliquam bibendum massa ut tristique. Integer pulvinar nunc dui, at blandit eros ultricies eget. Nulla risus libero, pharetra ut nisi luctus, commodo vestibulum velit. Vestibulum cursus sollicitudin egestas. Praesent ut dignissim ligula. Fusce posuere laoreet tempus. Vestibulum tincidunt placerat euismod. Suspendisse potenti.  Nunc facilisis velit ut tempor vestibulum.  Cras eget ligula diam. Nullam dignissim ac libero vitae aliquam. Nullam ullamcorper leo laoreet suscipit scelerisque. Ut nec bibendum elit, vitae malesuada massa. Vivamus ac libero et diam consectetur gravida non vitae nisl. Vestibulum et ex et ligula tincidunt pharetra. Vivamus congue libero risus, nec cursus tortor congue eget. Vivamus porttitor, metus vitae lobortis mattis, libero metus tincidunt velit, nec dapibus ipsum arcu vel odio. Vestibulum vitae diam vel quam tempus suscipit vel at arcu. Donec dui nibh, vehicula id scelerisque et, convallis eget nibh. Integer pharetra diam eget felis imperdiet, ut tempus libero porttitor. Aenean id orci sagittis odio vehicula mollis vel sed risus. Sed ante est, rutrum vel diam id, vehicula euismod lorem. Quisque lacus purus, suscipit et elit eget, interdum semper nibh. Phasellus facilisis, nibh eu tincidunt porttitor, magna diam aliquam magna, ut tincidunt erat magna pharetra est. Curabitur varius ante quis arcu dignissim, ac elementum nunc pharetra. Nulla finibus feugiat sapien, at tincidunt nisi pharetra vel. Sed in ultrices turpis. Curabitur ultrices fermentum lectus a luctus. Curabitur eget orci a mi consequat sodales.");
		trace("click tab [Contents]");
		tab("s3:s84");
		trace("set formatType (s3:s90) if it exists and is not disabled");
		selectOne("s3:s90", 1);
		trace("set sendFrom (s3:s102) if it exists and is not disabled");
		text("s3:s102", "Sed ante est, rutrum vel diam id, vehicula euismod lorem. Curabitur varius ante quis arcu dignissim, ac elementum nunc pharetra.");
		trace("set monitorBcc (s3:s108) if it exists and is not disabled");
		checkbox("s3:s108", Boolean.FALSE);
		trace("set sendTo (s3:s124) if it exists and is not disabled");
		text("s3:s124", "Vivamus vel neque pellentesque purus hendrerit finibus quis in magna. Vestibulum cursus sollicitudin egestas. Praesent ut dignissim ligula.");
		trace("set ccTo (s3:s131) if it exists and is not disabled");
		text("s3:s131", "Lorem ipsum dolor sit amet, consectetur adipiscing elit.  Cras eget ligula diam. Suspendisse ut mi felis.");
		trace("set subject (s3:s147) if it exists and is not disabled");
		text("s3:s147", "In cursus dignissim est ut varius.  Cras eget ligula diam. Vivamus ac libero et diam consectetur gravida non vitae nisl. Curabitur eget orci a mi consequat sodales.");
		trace("set template (s3:s170) if it exists and is not disabled");
		selectOne("s3:s170", 0);
		trace("set includeCalendar (s3:s181) if it exists and is not disabled");
		checkbox("s3:s181", Boolean.FALSE);
		trace("click tab [Options]");
		tab("s3:s195");
		trace("set attachmentFileName1 (s3:s213) if it exists and is not disabled");
		text("s3:s213", "Curabitur ultrices fermentum lectus a luctus. Donec convallis vitae leo eu hendrerit. Aenean id orci sagittis odio vehicula mollis vel sed risus.");
		trace("set attachmentFileName2 (s3:s232) if it exists and is not disabled");
		text("s3:s232", "Mauris sagittis.");
		trace("set attachmentFileName3 (s3:s251) if it exists and is not disabled");
		text("s3:s251", "In id est at arcu aliquam venenatis. Vestibulum et ex et ligula tincidunt pharetra. Vivamus congue libero risus, nec cursus tortor congue eget.");
		trace("set calendarTitleExpression (s3:s261) if it exists and is not disabled");
		text("s3:s261", "Nunc facilisis velit ut tempor vestibulum. Quisque varius, sem eu varius molestie, eros augue cursus purus, nec mattis leo mauris vitae nisi.");
		trace("set calendarStartTime (s3:s268) if it exists and is not disabled");
		_input("s3:s268", "");
		trace("set calendarEndTime (s3:s275) if it exists and is not disabled");
		_input("s3:s275", "");
		trace("set calendarDescriptionExpression (s3:s282) if it exists and is not disabled");
		text("s3:s282", "Phasellus dapibus mauris in mattis ultrices. Quisque lacus purus, suscipit et elit eget, interdum semper nibh. Donec eget elit neque. In id nisl elit.");
		trace("click tab [Subscriptions]");
		tab("s3:s288");
		trace("click [save] (s302) if it exists and is not disabled");
		button("s302", true, false);
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
		trace("click [AddUnsubscribeLink] (s3:s187) if it exists and is not disabled");
		button("s3:s187", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [AddImage] (s3:s191) if it exists and is not disabled");
		button("s3:s191", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Options]");
		tab("s3:s195");
		trace("click tab [Subscriptions]");
		tab("s3:s288");
		trace("click [delete] (s303) if it exists and is not disabled");
		redirectButton("s303", true);
		trace("Test Success");
		verifySuccess();
	}

	/**
	 * Menu Security Admin::Groups
	 */
	protected void testMenuSecurityAdminGroups() throws Exception {
		trace("List for default query of [admin.Group]");
		get("?a=l&m=admin&q=Group");
		listGridButton("s0", "s0:s4", false);
		trace("set name (s11) if it exists and is not disabled");
		text("s11", "Ut nec bibendum elit, vitae m");
		trace("set description (s18) if it exists and is not disabled");
		text("s18", "Curabitur varius quis quam a placerat. Ut nec bibendum elit, vitae malesuada massa. Suspendisse potenti.");
		trace("New row on data grid [roles] (s24)");
		dataGridButton("s24", "s24:s33", false);
		trace("set roleName (s7) if it exists and is not disabled");
		selectOne("s7", 11);
		trace("click [zoom out] (s17) if it exists and is not disabled");
		button("s17", false, false);
		trace("Test Success");
		verifySuccess();
		trace("click [save] (s46) if it exists and is not disabled");
		button("s46", true, false);
		trace("Test Success");
		verifySuccess();
		trace("set name (s11) if it exists and is not disabled");
		text("s11", "Quisque lacus purus,");
		trace("set description (s18) if it exists and is not disabled");
		text("s18", "Suspendisse ut mi felis. In sit amet mi eget libero varius pharetra sit amet quis dolor.  Aliquam mattis posuere imperdiet. Curabitur eget orci a mi consequat sodales.");
		trace("click [save] (s46) if it exists and is not disabled");
		button("s46", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [delete] (s47) if it exists and is not disabled");
		redirectButton("s47", true);
		trace("Test Success");
		verifySuccess();
	}

	/**
	 * Menu Security Admin::Data Groups
	 */
	protected void testMenuSecurityAdminDataGroups() throws Exception {
		trace("List for default query of [admin.DataGroup]");
		get("?a=l&m=admin&q=DataGroup");
		trace("set name (s7) if it exists and is not disabled");
		text("s7", "Praesent ut dignissim ligu");
		trace("set description (s14) if it exists and is not disabled");
		text("s14", "Phasellus at vehicula tortor.  Aliquam mattis posuere imperdiet.  In non pretium erat, at imperdiet neque. Mauris posuere erat at velit convallis, ut sodales lacus tristique. Nullam dignissim ac libero vitae aliquam. Nulla risus libero, pharetra ut nisi luctus, commodo vestibulum velit. Praesent ut dignissim ligula. In placerat interdum vulputate. Suspendisse ut mi felis.  Nunc facilisis velit ut tempor vestibulum. Vivamus porttitor, metus vitae lobortis mattis, libero metus tincidunt velit, nec dapibus ipsum arcu vel odio. Sed in nulla id nunc consectetur elementum ut non magna. Quisque varius, sem eu varius molestie, eros augue cursus purus, nec mattis leo mauris vitae nisi. Donec eget elit neque. Curabitur varius quis quam a placerat. Sed ante est, rutrum vel diam id, vehicula euismod lorem. In hac habitasse platea dictumst. Phasellus facilisis, nibh eu tincidunt porttitor, magna diam aliquam magna, ut tincidunt erat magna pharetra est. Aenean ut odio hendrerit, consequat orci a, ultrices massa. Vestibulum tincidunt placerat euismod. In id nisl elit. Nulla finibus feugiat sapien, at tincidunt nisi pharetra vel.  Etiam luctus cursus fermentum. Ut nec bibendum elit, vitae malesuada massa. Donec dui nibh, vehicula id scelerisque et, convallis eget nibh. Donec convallis vitae leo eu hendrerit.  Cras eget ligula diam. Phasellus dapibus mauris in mattis ultrices. Curabitur varius ante quis arcu dignissim, ac elementum nunc pharetra.");
		trace("click [save] (s21) if it exists and is not disabled");
		button("s21", true, false);
		trace("Test Success");
		verifySuccess();
		trace("set name (s7) if it exists and is not disabled");
		text("s7", "Nullam ullamcorper le");
		trace("set description (s14) if it exists and is not disabled");
		text("s14", "Nulla aliquam bibendum massa ut tristique. Fusce posuere laoreet tempus.Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam quis lorem in est dignissim semper in vitae mauris. Curabitur varius quis quam a placerat. Vivamus congue libero risus, nec cursus tortor congue eget. Aenean id orci sagittis odio vehicula mollis vel sed risus. Suspendisse potenti. Vivamus porttitor, metus vitae lobortis mattis, libero metus tincidunt velit, nec dapibus ipsum arcu vel odio. Vestibulum vitae diam vel quam tempus suscipit vel at arcu. Sed ante est, rutrum vel diam id, vehicula euismod lorem.  Nunc facilisis velit ut tempor vestibulum. Vestibulum et ex et ligula tincidunt pharetra. In id est at arcu aliquam venenatis. Sed in nulla id nunc consectetur elementum ut non magna. Curabitur aliquam accumsan purus, sed ultricies elit pulvinar in. Mauris posuere erat at velit convallis, ut sodales lacus tristique. Vivamus vel neque pellentesque purus hendrerit finibus quis in magna. Integer pulvinar nunc dui, at blandit eros ultricies eget. Suspendisse ut mi felis. Curabitur eget orci a mi consequat sodales. Aenean ut odio hendrerit, consequat orci a, ultrices massa. Pellentesque pellentesque, lectus at mattis egestas, metus nunc rutrum ipsum, ut imperdiet mi felis sed urna. Phasellus dapibus mauris in mattis ultrices. In cursus dignissim est ut varius.");
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
		selectOne("s3:s11", 30);
		trace("click tab [Backup/Restore]");
		tab("s3:s51");
		trace("set dailyBackupRetention (s3:s57) if it exists and is not disabled");
		_input("s3:s57", "2750");
		trace("set weeklyBackupRetention (s3:s63) if it exists and is not disabled");
		_input("s3:s63", "7516");
		trace("set monthlyBackupRetention (s3:s69) if it exists and is not disabled");
		_input("s3:s69", "7529");
		trace("set yearlyBackupRetention (s3:s75) if it exists and is not disabled");
		_input("s3:s75", "7112");
		trace("set contentRestoreOption (s3:s93) if it exists and is not disabled");
		trace("set restoreIndexingOption (s3:s101) if it exists and is not disabled");
		trace("set restorePreProcess (s3:s109) if it exists and is not disabled");
		selectOne("s3:s109", 1);
		trace("click tab [Data]");
		tab("s3:s140");
		trace("set schemaName (s3:s146) if it exists and is not disabled");
		text("s3:s146", "Vestibulum vitae diam vel quam tempus suscipit vel at arcu. Vivamus porttitor, metus vitae lobortis mattis, libero metus tincidunt velit, nec dapibus ipsum arcu vel odio. Sed ante est, rutrum vel diam id, vehicula euismod lorem. Maecenas vestibulum lacus id tellus eleifend, vel pellentesque ante lobortis.");
		trace("click tab [Content]");
		tab("s3:s175");
		trace("click tab [Manage Audits]");
		tab("s3:s228");
		trace("set auditModuleName (s3:s234) if it exists and is not disabled");
		selectOne("s3:s234", 2);
		trace("set auditDocumentName (s3:s242) if it exists and is not disabled");
		selectOne("s3:s242", 3);
		trace("set auditOperation (s3:s250) if it exists and is not disabled");
		selectOne("s3:s250", 2);
		trace("set auditTimestampStart (s3:s258) if it exists and is not disabled");
		_input("s3:s258", "17-Oct-2018 17:21:00");
		trace("set auditTimestampEnd (s3:s265) if it exists and is not disabled");
		_input("s3:s265", "17-Oct-2018 17:21:00");
		trace("set auditMatchCount (s3:s279) if it exists and is not disabled");
		text("s3:s279", "1927");
		trace("click tab [Refresh]");
		tab("s3:s322");
		trace("set refreshOption (s3:s344) if it exists and is not disabled");
		selectOne("s3:s344", 2);
		trace("set evictOption (s3:s351) if it exists and is not disabled");
		selectOne("s3:s351", 1);
		trace("set notification (s3:s358) if it exists and is not disabled");
		checkbox("s3:s358", Boolean.FALSE);
		trace("click tab [Export/Import]");
		tab("s3:s5");
		trace("click tab [Backup/Restore]");
		tab("s3:s51");
		trace("click tab [Data]");
		tab("s3:s140");
		trace("click tab [Content]");
		tab("s3:s175");
		trace("click tab [Manage Audits]");
		tab("s3:s228");
		trace("click tab [Refresh]");
		tab("s3:s322");
		trace("click [save] (s369) if it exists and is not disabled");
		button("s369", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Export/Import]");
		tab("s3:s5");
		trace("set modDocName (s3:s11) if it exists and is not disabled");
		selectOne("s3:s11", 23);
		trace("click tab [Backup/Restore]");
		tab("s3:s51");
		trace("set dailyBackupRetention (s3:s57) if it exists and is not disabled");
		_input("s3:s57", "3433");
		trace("set weeklyBackupRetention (s3:s63) if it exists and is not disabled");
		_input("s3:s63", "170");
		trace("set monthlyBackupRetention (s3:s69) if it exists and is not disabled");
		_input("s3:s69", "6638");
		trace("set yearlyBackupRetention (s3:s75) if it exists and is not disabled");
		_input("s3:s75", "6912");
		trace("set contentRestoreOption (s3:s93) if it exists and is not disabled");
		trace("set restoreIndexingOption (s3:s101) if it exists and is not disabled");
		trace("set restorePreProcess (s3:s109) if it exists and is not disabled");
		selectOne("s3:s109", 2);
		trace("click tab [Data]");
		tab("s3:s140");
		trace("set schemaName (s3:s146) if it exists and is not disabled");
		text("s3:s146", "Vivamus maximus tellus neque, nec vestibulum urna gravida et. Etiam feugiat ipsum sed tellus vulputate congue. Nullam dignissim ac libero vitae aliquam. Curabitur varius quis quam a placerat. Suspendisse varius sit amet lorem vitae efficitur. Vestibulum et ex et ligula tincidunt pharetra. Mauris posuere erat at velit convallis, ut sodales lacus tristique. Fusce posuere laoreet tempus. Donec sed feugiat neque. Nulla finibus feugiat sapien, at tincidunt nisi pharetra vel. In id est at arcu aliquam venenatis. Integer pharetra eget erat non ornare.Lorem ipsum dolor sit amet, consectetur adipiscing elit. In hac habitasse platea dictumst. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Curabitur eget orci a mi consequat sodales. Praesent ut dignissim ligula.  Nunc facilisis velit ut tempor vestibulum.");
		trace("click tab [Content]");
		tab("s3:s175");
		trace("click tab [Manage Audits]");
		tab("s3:s228");
		trace("set auditModuleName (s3:s234) if it exists and is not disabled");
		selectOne("s3:s234", 3);
		trace("set auditDocumentName (s3:s242) if it exists and is not disabled");
		selectOne("s3:s242", 1);
		trace("set auditOperation (s3:s250) if it exists and is not disabled");
		selectOne("s3:s250", 1);
		trace("set auditTimestampStart (s3:s258) if it exists and is not disabled");
		_input("s3:s258", "17-Oct-2018 17:21:00");
		trace("set auditTimestampEnd (s3:s265) if it exists and is not disabled");
		_input("s3:s265", "17-Oct-2018 17:21:00");
		trace("set auditMatchCount (s3:s279) if it exists and is not disabled");
		text("s3:s279", "8174");
		trace("click tab [Refresh]");
		tab("s3:s322");
		trace("set refreshOption (s3:s344) if it exists and is not disabled");
		selectOne("s3:s344", 2);
		trace("set evictOption (s3:s351) if it exists and is not disabled");
		selectOne("s3:s351", 3);
		trace("set notification (s3:s358) if it exists and is not disabled");
		checkbox("s3:s358", Boolean.FALSE);
		trace("click [save] (s369) if it exists and is not disabled");
		button("s369", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Export/Import]");
		tab("s3:s5");
		trace("click tab [Backup/Restore]");
		tab("s3:s51");
		trace("click [Backup] (s3:s131) if it exists and is not disabled");
		button("s3:s131", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [DownloadBackup] (s3:s134) if it exists and is not disabled");
		button("s3:s134", true, true);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Data]");
		tab("s3:s140");
		trace("click [Create] (s3:s169) if it exists and is not disabled");
		button("s3:s169", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [Drop] (s3:s171) if it exists and is not disabled");
		button("s3:s171", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [Sync] (s3:s173) if it exists and is not disabled");
		button("s3:s173", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Content]");
		tab("s3:s175");
		trace("click [CheckContent] (s3:s204) if it exists and is not disabled");
		button("s3:s204", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [Reindex] (s3:s224) if it exists and is not disabled");
		button("s3:s224", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Manage Audits]");
		tab("s3:s228");
		trace("click [CheckAuditMatches] (s3:s285) if it exists and is not disabled");
		button("s3:s285", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Refresh]");
		tab("s3:s322");
		trace("click [RefreshDocumentTuples] (s3:s364) if it exists and is not disabled");
		button("s3:s364", true, true);
		trace("Test Success");
		verifySuccess();
		trace("click [save] (s369) if it exists and is not disabled");
		button("s369", true, false);
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
		text("s3:s11", "Donec eget elit neque. In cursus dignissim est ut varius. Integer pulvinar nunc dui, at blandit eros ultricies eget. Nulla aliquam bibendum massa ut tristique. Integer pharetra eget erat non ornare. Curabitur id pretium nulla. Suspendisse varius sit amet lorem vitae efficitur. Curabitur eget orci a mi consequat sodales. Aenean ut odio hendrerit, consequat orci a, ultrices massa. Vivamus porttitor, metus vitae lobortis mattis, libero metus tincidunt velit, nec dapibus ipsum arcu vel odio. Nulla finibus feugiat sapien, at tincidunt nisi pharetra vel. Phasellus at vehicula tortor.  Etiam luctus cursus fermentum. In hac habitasse platea dictumst. Vivamus maximus tellus neque, nec vestibulum urna gravida et. Nulla risus libero, pharetra ut nisi luctus, commodo vestibulum velit. Mauris posuere erat at velit convallis, ut sodales lacus tristique. Duis eget quam vehicula, hendrerit leo ac, ullamcorper augue. Fusce posuere laoreet tempus. Phasellus facilisis, nibh eu tincidunt porttitor, magna diam aliquam magna, ut tincidunt erat magna pharetra est. Vivamus ac libero et diam consectetur gravida non vitae nisl. Ut nec bibendum elit, vitae malesuada massa. Sed ante est, rutrum vel diam id, vehicula euismod lorem. Pellentesque pellentesque, lectus at mattis egestas, metus nunc rutrum ipsum, ut imperdiet mi felis sed urna. Suspendisse ut mi felis. Vestibulum tincidunt placerat euismod. Suspendisse potenti. Curabitur ultrices fermentum lectus a luctus. Duis nec tincidunt velit, id vestibulum sem. Nullam dignissim ac libero vitae aliquam.");
		trace("set outputLocation (s3:s18) if it exists and is not disabled");
		text("s3:s18", "Curabitur varius quis quam a placerat. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Suspendisse varius sit amet lorem vitae efficitur.");
		trace("set defaultModule (s3:s25) if it exists and is not disabled");
		selectOne("s3:s25", 2);
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
		trace("set moduleName (s7) if it exists and is not disabled");
		text("s7", "Etiam feugiat ipsum sed tellus vulputate congue.");
		trace("set queryName (s14) if it exists and is not disabled");
		text("s14", "Curabitur eget orci a mi consequat sodales.");
		trace("set name (s21) if it exists and is not disabled");
		text("s21", "Aliquam quis lorem in est dignissim semper in vitae mauris. In sit amet mi eget libero varius pharetra sit amet quis dolor.");
		trace("set snapshot (s28) if it exists and is not disabled");
		text("s28", "Proin nec pharetra orci. Sed quis odio quam. Mauris posuere erat at velit convallis, ut sodales lacus tristique. Donec convallis vitae leo eu hendrerit. Mauris sagittis. Nullam ullamcorper leo laoreet suscipit scelerisque. Aenean ut odio hendrerit, consequat orci a, ultrices massa. Vivamus vel neque pellentesque purus hendrerit finibus quis in magna. In hac habitasse platea dictumst. Curabitur varius ante quis arcu dignissim, ac elementum nunc pharetra. Integer pharetra eget erat non ornare.  Nunc facilisis velit ut tempor vestibulum. Nullam dignissim ac libero vitae aliquam. Quisque lacus purus, suscipit et elit eget, interdum semper nibh. Curabitur id pretium nulla. Nulla risus libero, pharetra ut nisi luctus, commodo vestibulum velit. Cras venenatis sit amet urna a eleifend. Vestibulum et ex et ligula tincidunt pharetra. Ut nec bibendum elit, vitae malesuada massa. Duis nec tincidunt velit, id vestibulum sem. In placerat interdum vulputate. Vivamus congue libero risus, nec cursus tortor congue eget. Curabitur varius quis quam a placerat.  In non pretium erat, at imperdiet neque. In id est at arcu aliquam venenatis. Curabitur eget orci a mi consequat sodales. Pellentesque pellentesque, lectus at mattis egestas, metus nunc rutrum ipsum, ut imperdiet mi felis sed urna. Integer pulvinar nunc dui, at blandit eros ultricies eget. Vestibulum vitae diam vel quam tempus suscipit vel at arcu.  Cras eget ligula diam. Vestibulum tincidunt placerat euismod.  Aliquam mattis posuere imperdiet. Phasellus at vehicula tortor. Nulla finibus feugiat sapien, at tincidunt nisi pharetra vel. Sed in ultrices turpis. Aenean id orci sagittis odio vehicula mollis vel sed risus. Fusce posuere laoreet tempus. Donec dui nibh, vehicula id scelerisque et, convallis eget nibh. Vestibulum cursus sollicitudin egestas. Phasellus dapibus mauris in mattis ultrices. Curabitur aliquam accumsan purus, sed ultricies elit pulvinar in. Praesent ut dignissim ligula. In id nisl elit.");
		trace("set copyToUserSnapshotName (s45) if it exists and is not disabled");
		text("s45", "Integer pharetra diam eget felis imperdiet, ut tempus libero porttitor. Vivamus ac libero et diam consectetur gravida non vitae nisl. Donec eget elit neque.");
		trace("click [save] (s61) if it exists and is not disabled");
		button("s61", true, false);
		trace("Test Success");
		verifySuccess();
		trace("set moduleName (s7) if it exists and is not disabled");
		text("s7", "Donec eget elit neque.");
		trace("set queryName (s14) if it exists and is not disabled");
		text("s14", "Phasellus dapibus mauris in mattis ul");
		trace("set name (s21) if it exists and is not disabled");
		text("s21", "Nullam ullamcorper leo laoreet suscipit scelerisque.  Etiam luctus cursus fermentum. Vivamus congue libero risus, nec cursus tortor congue eget.");
		trace("set snapshot (s28) if it exists and is not disabled");
		text("s28", "Nullam ullamcorper leo laoreet suscipit scelerisque. Suspendisse ut mi felis. Integer pulvinar nunc dui, at blandit eros ultricies eget. Nulla finibus feugiat sapien, at tincidunt nisi pharetra vel. Integer pharetra diam eget felis imperdiet, ut tempus libero porttitor. Phasellus at vehicula tortor. Vivamus congue libero risus, nec cursus tortor congue eget. In placerat interdum vulputate. Vivamus vel neque pellentesque purus hendrerit finibus quis in magna. Suspendisse varius sit amet lorem vitae efficitur. Nullam dignissim ac libero vitae aliquam. Aliquam quis lorem in est dignissim semper in vitae mauris.  Aliquam mattis posuere imperdiet. Quisque varius, sem eu varius molestie, eros augue cursus purus, nec mattis leo mauris vitae nisi. Curabitur id pretium nulla. Nulla risus libero, pharetra ut nisi luctus, commodo vestibulum velit. In cursus dignissim est ut varius. Proin nec pharetra orci. In sit amet mi eget libero varius pharetra sit amet quis dolor. Phasellus dapibus mauris in mattis ultrices. In id est at arcu aliquam venenatis. Phasellus facilisis, nibh eu tincidunt porttitor, magna diam aliquam magna, ut tincidunt erat magna pharetra est. Integer pharetra eget erat non ornare. Vestibulum et ex et ligula tincidunt pharetra. Praesent ut dignissim ligula. Duis nec tincidunt velit, id vestibulum sem. Cras venenatis sit amet urna a eleifend. Etiam feugiat ipsum sed tellus vulputate congue. In id nisl elit. Aenean id orci sagittis odio vehicula mollis vel sed risus.");
		trace("set copyToUserSnapshotName (s45) if it exists and is not disabled");
		text("s45", "Praesent ut dignissim ligula. Proin nec pharetra orci. Suspendisse varius sit amet lorem vitae efficitur. Integer pharetra eget erat non ornare. Sed ante est, rutrum vel diam id, vehicula euismod lorem.");
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
		listGridButton("s0", "s0:s6", false);
		trace("set moduleName (s7) if it exists and is not disabled");
		text("s7", "Proin nec pharetra orci. Curabitur eget orci a mi consequat sodales. Duis nec tincidunt velit, id vestibulum sem. Vestibulum et ex et ligula tincidunt pharetra. Quisque lacus purus, suscipit et elit eget, interdum semper nibh. Nullam ullamcorper leo laoreet suscipit scelerisque. Vestibulum cursus sollicitudin egestas. Sed ante est, rutrum vel diam id, vehicula euismod lorem. Praesent ut dignissim ligula. Suspendisse ut mi felis.");
		trace("set documentName (s14) if it exists and is not disabled");
		text("s14", "Mauris posuere erat at velit convallis, ut sodales lacus tristique. Vivamus vel neque pellentesque purus hendrerit finibus quis in magna. Quisque varius, sem eu varius molestie, eros augue cursus purus, nec mattis leo mauris vitae nisi. Proin nec pharetra orci. Curabitur aliquam accumsan purus, sed ultricies elit pulvinar in. Vestibulum tincidunt placerat euismod. Sed in ultrices turpis. Suspendisse varius sit amet lorem vitae efficitur. Sed quis odio quam.");
		trace("set sequenceName (s21) if it exists and is not disabled");
		text("s21", "Fusce posuere laoreet tempus. Vivamus vel neque pellentesque purus hendrerit finibus quis in magna. Curabitur varius ante quis arcu dignissim, ac elementum nunc pharetra. Duis nec tincidunt velit, id vestibulum sem.  Aliquam mattis posuere imperdiet. Ut nec bibendum elit, vitae malesuada massa. Nulla aliquam bibendum massa ut tristique.");
		trace("set documentNumber (s28) if it exists and is not disabled");
		text("s28", "Integer pharetra eget erat non ornare. Curabitur varius quis quam a placerat. Cras venenatis sit amet urna a eleifend.");
		trace("click [save] (s35) if it exists and is not disabled");
		button("s35", true, false);
		trace("Test Success");
		verifySuccess();
		trace("set moduleName (s7) if it exists and is not disabled");
		text("s7", "In placerat interdum vulputate. Phasellus at vehicula tortor. In id est at arcu aliquam venenatis. Donec dui nibh, vehicula id scelerisque et, convallis eget nibh. Etiam feugiat ipsum sed tellus vulputate congue. Nulla risus libero, pharetra ut nisi luctus, commodo vestibulum velit. Sed ante est, rutrum vel diam id, vehicula euismod lorem. Aenean ut odio hendrerit, consequat orci a, ultrices massa.");
		trace("set documentName (s14) if it exists and is not disabled");
		text("s14", "Sed in ultrices turpis. Ut nec bibendum elit, vitae malesuada massa.  Etiam luctus cursus fermentum.  In non pretium erat, at imperdiet neque. Cras venenatis sit amet urna a eleifend. Curabitur varius quis quam a placerat. In sit amet mi eget libero varius pharetra sit amet quis dolor. Suspendisse varius sit amet lorem vitae efficitur. In id nisl elit.");
		trace("set sequenceName (s21) if it exists and is not disabled");
		text("s21", "Sed in ultrices turpis.  Nunc facilisis velit ut tempor vestibulum. Quisque varius, sem eu varius molestie, eros augue cursus purus, nec mattis leo mauris vitae nisi. Praesent ut dignissim ligula. Curabitur aliquam accumsan purus, sed ultricies elit pulvinar in. In sit amet mi eget libero varius pharetra sit amet quis dolor. Nulla risus libero, pharetra ut nisi luctus, commodo vestibulum velit. Sed quis odio quam. Vestibulum et ex et ligula tincidunt pharetra.");
		trace("set documentNumber (s28) if it exists and is not disabled");
		text("s28", "Sed in nulla id nunc consectetur elementum ut non magna. Vestibulum cursus sollicitudin egestas. Curabitur id pretium nulla. Praesent ut dignissim ligula. Nullam ullamcorper leo laoreet suscipit scelerisque. Curabitur ultrices fermentum lectus a luctus. Phasellus dapibus mauris in mattis ultrices. Fusce posuere laoreet tempus. Donec sed feugiat neque. In sit amet mi eget libero varius pharetra sit amet quis dolor.");
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
