package sail.admin;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import util.sail.BrowserConfiguration;
import util.sail.Devices;
import util.sail.PrimeFacesTest;

public class AdminSail extends PrimeFacesTest {

	private String pathToChromeDriver = "/C:/Windows/chromedriver.exe";
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
	
	/**
	 * Login
	 */
	protected void testLogin() throws Exception {
		trace("Login as demo/admin");
		login("demo", "admin", "admin");
	}

	/**
	 * Edit document ChangePassword
	 */
	protected void testMenuPassword() throws Exception {
		trace("Edit new document [admin.ChangePassword] instance");
		get("?a=e&m=admin&d=ChangePassword");
		trace("set oldPassword (s8password) if it exists and is not disabled");
		text("s8password", "setup");
		trace("set newPassword (s16password) if it exists and is not disabled");
		text("s16password", "password01");
		trace("click [MakePasswordChange] (s36) if it exists and is not disabled");
		button("s36", true, false);
		trace("Test Success");
		verifySuccess();
		trace("Test Failure");
		verifyFailure();
		trace("set oldPassword (s8password) if it exists and is not disabled");
		text("s8password", "setup");
		trace("set confirmPassword (s24password) if it exists and is not disabled");
		text("s24password", "password01");
		trace("click [MakePasswordChange] (s36) if it exists and is not disabled");
		button("s36", true, false);
		trace("Test Success");
		verifySuccess();
		trace("Test Failure");
		verifyFailure();
		trace("set newPassword (s16password) if it exists and is not disabled");
		text("s16password", "password01");
		trace("set confirmPassword (s24password) if it exists and is not disabled");
		text("s24password", "password01");
		trace("click [MakePasswordChange] (s36) if it exists and is not disabled");
		button("s36", true, false);
		trace("Test Success");
		verifySuccess();
		trace("Test Failure");
		verifyFailure();
		trace("Edit new document [admin.Configuration] instance");
		get("?a=e&m=admin&d=Configuration");
		trace("set passwordMinLength (s3:s32) if it exists and is not disabled");
		_input("s3:s32", "12");
		trace("click [save] (s435) if it exists and is not disabled");
		button("s435", true, false);
		trace("Test Success");
		verifySuccess();
		trace("Edit new document [admin.ChangePassword] instance");
		get("?a=e&m=admin&d=ChangePassword");
		trace("set oldPassword (s8password) if it exists and is not disabled");
		text("s8password", "setup");
		trace("set newPassword (s16password) if it exists and is not disabled");
		text("s16password", "password01#");
		trace("set confirmPassword (s24password) if it exists and is not disabled");
		text("s24password", "password01#");
		trace("click [MakePasswordChange] (s36) if it exists and is not disabled");
		button("s36", true, false);
		trace("Test Success");
		verifySuccess();
		trace("Test Failure");
		verifyFailure();
		trace("set oldPassword (s8password) if it exists and is not disabled");
		text("s8password", "setup");
		trace("set newPassword (s16password) if it exists and is not disabled");
		text("s16password", "password01#123");
		trace("set confirmPassword (s24password) if it exists and is not disabled");
		text("s24password", "password02#123");
		trace("click [MakePasswordChange] (s36) if it exists and is not disabled");
		button("s36", true, false);
		trace("Test Success");
		verifySuccess();
		trace("Test Failure");
		verifyFailure();
		trace("Edit new document [admin.Configuration] instance");
		get("?a=e&m=admin&d=Configuration");
		trace("set passwordRequireLowercase (s3:s40) if it exists and is not disabled");
		checkbox("s3:s40", Boolean.TRUE);
		trace("click [save] (s435) if it exists and is not disabled");
		button("s435", true, false);
		trace("Test Success");
		verifySuccess();
		trace("Edit new document [admin.ChangePassword] instance");
		get("?a=e&m=admin&d=ChangePassword");
		trace("set oldPassword (s8password) if it exists and is not disabled");
		text("s8password", "setup");
		trace("set newPassword (s16password) if it exists and is not disabled");
		text("s16password", "PASSWORD01#123");
		trace("set confirmPassword (s24password) if it exists and is not disabled");
		text("s24password", "PASSWORD01#123");
		trace("click [MakePasswordChange] (s36) if it exists and is not disabled");
		button("s36", true, false);
		trace("Test Success");
		verifySuccess();
		trace("Test Failure");
		verifyFailure();
		trace("Edit new document [admin.Configuration] instance");
		get("?a=e&m=admin&d=Configuration");
		trace("set passwordRequireLowercase (s3:s40) if it exists and is not disabled");
		checkbox("s3:s40", Boolean.FALSE);
		trace("set passwordRequireUppercase (s3:s46) if it exists and is not disabled");
		checkbox("s3:s46", Boolean.TRUE);
		trace("click [save] (s435) if it exists and is not disabled");
		button("s435", true, false);
		trace("Test Success");
		verifySuccess();
		trace("Edit new document [admin.ChangePassword] instance");
		get("?a=e&m=admin&d=ChangePassword");
		trace("set oldPassword (s8password) if it exists and is not disabled");
		text("s8password", "setup");
		trace("set newPassword (s16password) if it exists and is not disabled");
		text("s16password", "password01#123");
		trace("set confirmPassword (s24password) if it exists and is not disabled");
		text("s24password", "password01#123");
		trace("click [MakePasswordChange] (s36) if it exists and is not disabled");
		button("s36", true, false);
		trace("Test Success");
		verifySuccess();
		trace("Test Failure");
		verifyFailure();
		trace("Edit new document [admin.Configuration] instance");
		get("?a=e&m=admin&d=Configuration");
		trace("set passwordRequireUppercase (s3:s46) if it exists and is not disabled");
		checkbox("s3:s46", Boolean.FALSE);
		trace("set passwordRequireNumeric (s3:s54) if it exists and is not disabled");
		checkbox("s3:s54", Boolean.TRUE);
		trace("click [save] (s435) if it exists and is not disabled");
		button("s435", true, false);
		trace("Test Success");
		verifySuccess();
		trace("Edit new document [admin.ChangePassword] instance");
		get("?a=e&m=admin&d=ChangePassword");
		trace("set oldPassword (s8password) if it exists and is not disabled");
		text("s8password", "setup");
		trace("set newPassword (s16password) if it exists and is not disabled");
		text("s16password", "passwordNumber#");
		trace("set confirmPassword (s24password) if it exists and is not disabled");
		text("s24password", "passwordNumber#");
		trace("click [MakePasswordChange] (s36) if it exists and is not disabled");
		button("s36", true, false);
		trace("Test Success");
		verifySuccess();
		trace("Test Failure");
		verifyFailure();
		trace("Edit new document [admin.Configuration] instance");
		get("?a=e&m=admin&d=Configuration");
		trace("set passwordRequireNumeric (s3:s54) if it exists and is not disabled");
		checkbox("s3:s54", Boolean.FALSE);
		trace("set passwordRequireSpecial (s3:s60) if it exists and is not disabled");
		checkbox("s3:s60", Boolean.TRUE);
		trace("click [save] (s435) if it exists and is not disabled");
		button("s435", true, false);
		trace("Test Success");
		verifySuccess();
		trace("Edit new document [admin.ChangePassword] instance");
		get("?a=e&m=admin&d=ChangePassword");
		trace("set oldPassword (s8password) if it exists and is not disabled");
		text("s8password", "setup");
		trace("set newPassword (s16password) if it exists and is not disabled");
		text("s16password", "passwordNumber01");
		trace("set confirmPassword (s24password) if it exists and is not disabled");
		text("s24password", "passwordNumber01");
		trace("click [MakePasswordChange] (s36) if it exists and is not disabled");
		button("s36", true, false);
		trace("Test Success");
		verifySuccess();
		trace("Test Failure");
		verifyFailure();
		trace("Edit new document [admin.Configuration] instance");
		get("?a=e&m=admin&d=Configuration");
		trace("set passwordRequireSpecial (s3:s60) if it exists and is not disabled");
		checkbox("s3:s60", Boolean.FALSE);
		trace("click [save] (s435) if it exists and is not disabled");
		button("s435", true, false);
		trace("Test Success");
		verifySuccess();
	}

	/**
	 * Menu User Dashboard
	 */
	protected void testMenuUserDashboard() throws Exception {
		trace("Edit new document [admin.UserDashboard] instance");
		get("?a=e&m=admin&d=UserDashboard");
		trace("set currentUser.userName (s18) if it exists and is not disabled");
		text("s18", "admin");
		trace("set currentUser.contact.name (s26) if it exists and is not disabled");
		text("s26", "admin");
		trace("set currentUser.contact.email1 (s34) if it exists and is not disabled");
		text("s34", "admin@yourdomain.com.au");
		trace("set currentUser.contact.mobile (s42) if it exists and is not disabled");
		text("s42", "");
		trace("click [UpdateMyDetails] (s54) if it exists and is not disabled");
		button("s54", true, false);
		trace("Test Success");
		verifySuccess();
	}

	/**
	 * Menu Contacts
	 */
	protected void testMenuContacts() throws Exception {
		trace("List for default query of [admin.Contact]");
		get("?a=l&m=admin&q=Contact");
		trace("New row on list grid [admin.Contact] (s0)");
		listGridButton("s0", "s0:s8", false);
		trace("set name (s10) if it exists and is not disabled");
		text("s10", "Sharan Wodicka");
		trace("set contactType (s18) if it exists and is not disabled");
		selectOne("s18", 2);
		trace("set email1 (s27) if it exists and is not disabled");
		text("s27", "zwarman@gmail.com");
		trace("set mobile (s35) if it exists and is not disabled");
		text("s35", "0414 731 630");
		trace("click [save] (s60) if it exists and is not disabled");
		button("s60", true, false);
		trace("Test Success");
		verifySuccess();
		trace("set name (s10) if it exists and is not disabled");
		text("s10", "Emmanuel Avera");
		trace("set contactType (s18) if it exists and is not disabled");
		selectOne("s18", 2);
		trace("set email1 (s27) if it exists and is not disabled");
		text("s27", "della.selestewa@gmail.com");
		trace("set mobile (s35) if it exists and is not disabled");
		text("s35", "0415 230 654");
		trace("click [save] (s60) if it exists and is not disabled");
		button("s60", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [delete] (s61) if it exists and is not disabled");
		redirectButton("s61", true);
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
		trace("click tab [Contents]");
		tab("s13:s15");
		trace("set description (s13:s22) if it exists and is not disabled");
		text("s13:s22", "Fusce posuere laoreet tempus.  Cras eget ligula diam. Ut nec bibendum elit, vitae malesuada massa.");
		trace("set systemUse (s13:s28) if it exists and is not disabled");
		checkbox("s13:s28", Boolean.FALSE);
		trace("set formatType (s13:s39) if it exists and is not disabled");
		selectOne("s13:s39", 1);
		trace("set sendFrom (s13:s48) if it exists and is not disabled");
		text("s13:s48", "Sed ante est, rutrum vel diam id, vehicula euismod lorem. In cursus dignissim est ut varius.");
		trace("set monitorBcc (s13:s54) if it exists and is not disabled");
		checkbox("s13:s54", Boolean.FALSE);
		trace("set sendTo (s13:s72) if it exists and is not disabled");
		text("s13:s72", "Vivamus maximus tellus neque, nec vestibulum urna gravida et. In hac habitasse platea dictumst.");
		trace("set ccTo (s13:s80) if it exists and is not disabled");
		text("s13:s80", "Mauris posuere erat at velit convallis, ut sodales lacus tristique. Aenean id orci sagittis odio vehicula mollis vel sed risus.");
		trace("set subject (s13:s98) if it exists and is not disabled");
		text("s13:s98", "Donec convallis vitae leo eu hendrerit. Duis nec tincidunt velit, id vestibulum sem. Nullam dignissim ac libero vitae aliquam.");
		trace("set body (s13:s116) if it exists and is not disabled");
		text("s13:s116", "Sed in nulla id nunc consectetur elementum ut non magna. Phasellus at vehicula tortor. Vestibulum cursus sollicitudin egestas. Curabitur varius ante quis arcu dignissim, ac elementum nunc pharetra. Nulla aliquam bibendum massa ut tristique. Vivamus maximus tellus neque, nec vestibulum urna gravida et. Sed ante est, rutrum vel diam id, vehicula euismod lorem. Sed in ultrices turpis. Nulla finibus feugiat sapien, at tincidunt nisi pharetra vel. In sit amet mi eget libero varius pharetra sit amet quis dolor. Aenean ut odio hendrerit, consequat orci a, ultrices massa. Sed quis odio quam. Quisque varius, sem eu varius molestie, eros augue cursus purus, nec mattis leo mauris vitae nisi. Donec convallis vitae leo eu hendrerit. Donec dui nibh, vehicula id scelerisque et, convallis eget nibh. Vestibulum et ex et ligula tincidunt pharetra. Nullam ullamcorper leo laoreet suscipit scelerisque. Curabitur eget orci a mi consequat sodales. Phasellus facilisis, nibh eu tincidunt porttitor, magna diam aliquam magna, ut tincidunt erat magna pharetra est.  Cras eget ligula diam. Donec eget elit neque. In id nisl elit. Phasellus dapibus mauris in mattis ultrices. Nullam dignissim ac libero vitae aliquam. Curabitur ultrices fermentum lectus a luctus. Suspendisse ut mi felis. Duis eget quam vehicula, hendrerit leo ac, ullamcorper augue. Proin nec pharetra orci. Suspendisse potenti. Aenean id orci sagittis odio vehicula mollis vel sed risus. Vivamus vel neque pellentesque purus hendrerit finibus quis in magna.");
		trace("set template (s13:s124) if it exists and is not disabled");
		selectOne("s13:s124", 0);
		trace("set includeCalendar (s13:s136) if it exists and is not disabled");
		checkbox("s13:s136", Boolean.FALSE);
		trace("click tab [Attachments]");
		tab("s13:s154");
		trace("set attachmentFileName1 (s13:s173) if it exists and is not disabled");
		text("s13:s173", "Suspendisse ut mi felis.  In non pretium erat, at imperdiet neque.");
		trace("set attachmentFileName2 (s13:s193) if it exists and is not disabled");
		text("s13:s193", "In cursus dignissim est ut varius. Nulla risus libero, pharetra ut nisi luctus, commodo vestibulum velit. Nullam ullamcorper leo laoreet suscipit scelerisque.");
		trace("set attachmentFileName3 (s13:s213) if it exists and is not disabled");
		text("s13:s213", "Pellentesque pellentesque, lectus at mattis egestas, metus nunc rutrum ipsum, ut imperdiet mi felis sed urna.");
		trace("set calendarTitleExpression (s13:s224) if it exists and is not disabled");
		text("s13:s224", "Etiam luctus cursus fermentum. Donec dui nibh, vehicula id scelerisque et, convallis eget nibh.");
		trace("set calendarStartTime (s13:s232) if it exists and is not disabled");
		_input("s13:s232", "15-Dec-2020 23:19");
		trace("set calendarEndTime (s13:s240) if it exists and is not disabled");
		_input("s13:s240", "15-Dec-2020 23:19");
		trace("set calendarDescriptionExpression (s13:s248) if it exists and is not disabled");
		text("s13:s248", "Cras venenatis sit amet urna a eleifend. Integer pharetra eget erat non ornare. Nullam ullamcorper leo laoreet suscipit scelerisque. Aenean ut odio hendrerit, consequat orci a, ultrices massa. Phasellus dapibus mauris in mattis ultrices. Aenean id orci sagittis odio vehicula mollis vel sed risus.");
		trace("click tab [Manage]");
		tab("s13:s254");
		trace("set moduleName (s13:s261) if it exists and is not disabled");
		selectOne("s13:s261", 1);
		trace("set documentName (s13:s270) if it exists and is not disabled");
		selectOne("s13:s270", 13);
		trace("set tag (s13:s285) if it exists and is not disabled");
		selectOne("s13:s285", 0);
		trace("set unTagSuccessful (s13:s294) if it exists and is not disabled");
		checkbox("s13:s294", Boolean.FALSE);
		trace("set notification (s13:s302) if it exists and is not disabled");
		checkbox("s13:s302", Boolean.FALSE);
		trace("set results (s13:s344) if it exists and is not disabled");
		text("s13:s344", "Mauris posuere erat at velit convallis, ut sodales lacus tristique. Cras venenatis sit amet urna a eleifend. Vestibulum tincidunt placerat euismod.  Nunc facilisis velit ut tempor vestibulum.  Cras eget ligula diam. In sit amet mi eget libero varius pharetra sit amet quis dolor. Curabitur ultrices fermentum lectus a luctus. Pellentesque pellentesque, lectus at mattis egestas, metus nunc rutrum ipsum, ut imperdiet mi felis sed urna. Integer pharetra diam eget felis imperdiet, ut tempus libero porttitor. Quisque varius, sem eu varius molestie, eros augue cursus purus, nec mattis leo mauris vitae nisi.  Etiam luctus cursus fermentum. Curabitur aliquam accumsan purus, sed ultricies elit pulvinar in. Sed ante est, rutrum vel diam id, vehicula euismod lorem.  In non pretium erat, at imperdiet neque. Ut nec bibendum elit, vitae malesuada massa. Aenean ut odio hendrerit, consequat orci a, ultrices massa.");
		trace("click tab [Subscriptions]");
		tab("s13:s365");
		trace("click tab [Contents]");
		tab("s13:s15");
		trace("click tab [Attachments]");
		tab("s13:s154");
		trace("click tab [Manage]");
		tab("s13:s254");
		trace("click tab [Subscriptions]");
		tab("s13:s365");
		trace("click [save] (s376) if it exists and is not disabled");
		button("s376", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Contents]");
		tab("s13:s15");
		trace("set description (s13:s22) if it exists and is not disabled");
		text("s13:s22", "Aenean id orci sagittis odio vehicula mollis vel sed risus.");
		trace("set systemUse (s13:s28) if it exists and is not disabled");
		checkbox("s13:s28", Boolean.FALSE);
		trace("set formatType (s13:s39) if it exists and is not disabled");
		selectOne("s13:s39", 1);
		trace("set sendFrom (s13:s48) if it exists and is not disabled");
		text("s13:s48", "Nulla finibus feugiat sapien, at tincidunt nisi pharetra vel. Curabitur eget orci a mi consequat sodales.  Etiam luctus cursus fermentum.");
		trace("set monitorBcc (s13:s54) if it exists and is not disabled");
		checkbox("s13:s54", Boolean.FALSE);
		trace("set sendTo (s13:s72) if it exists and is not disabled");
		text("s13:s72", "Nulla aliquam bibendum massa ut tristique.");
		trace("set ccTo (s13:s80) if it exists and is not disabled");
		text("s13:s80", "In sit amet mi eget libero varius pharetra sit amet quis dolor.");
		trace("set subject (s13:s98) if it exists and is not disabled");
		text("s13:s98", "In sit amet mi eget libero varius pharetra sit amet quis dolor.");
		trace("set body (s13:s116) if it exists and is not disabled");
		text("s13:s116", "Fusce posuere laoreet tempus.Lorem ipsum dolor sit amet, consectetur adipiscing elit. Duis eget quam vehicula, hendrerit leo ac, ullamcorper augue. Phasellus at vehicula tortor. Cras venenatis sit amet urna a eleifend. Sed quis odio quam. Donec eget elit neque. Curabitur varius ante quis arcu dignissim, ac elementum nunc pharetra. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Curabitur eget orci a mi consequat sodales. Donec convallis vitae leo eu hendrerit. Vivamus porttitor, metus vitae lobortis mattis, libero metus tincidunt velit, nec dapibus ipsum arcu vel odio. Etiam feugiat ipsum sed tellus vulputate congue. Integer pulvinar nunc dui, at blandit eros ultricies eget. Ut nec bibendum elit, vitae malesuada massa. Duis nec tincidunt velit, id vestibulum sem.");
		trace("set template (s13:s124) if it exists and is not disabled");
		selectOne("s13:s124", 0);
		trace("set includeCalendar (s13:s136) if it exists and is not disabled");
		checkbox("s13:s136", Boolean.FALSE);
		trace("click tab [Attachments]");
		tab("s13:s154");
		trace("set attachmentFileName1 (s13:s173) if it exists and is not disabled");
		text("s13:s173", "Curabitur eget orci a mi consequat sodales. In cursus dignissim est ut varius.");
		trace("set attachmentFileName2 (s13:s193) if it exists and is not disabled");
		text("s13:s193", "In non pretium erat, at imperdiet neque.  Cras eget ligula diam. Vivamus congue libero risus, nec cursus tortor congue eget. Mauris sagittis. Vestibulum cursus sollicitudin egestas.");
		trace("set attachmentFileName3 (s13:s213) if it exists and is not disabled");
		text("s13:s213", "ANOOUHMKXGJBWGETARVFHGOXRXGDAJKDTIHIWDNWYMHPNHZHHLQSBLZLHEJABKRKUXSXQOWQUFASTWBIUKUEILMQQZTKZASVHJNYPTZJBJYXSUAPAGKTAWXYHKSWUFNDSMYIFNCJPIVAOPTNDZKMVDTXBACBOUHMEHDVJYXWYPBBQNXYCDNNERSZPNFAPNKOLWQTZDQHWXCEATURYBJGSLDLYIQAGKBTVFIUXRFVERWYIZJMLOCDOVVRTQ");
		trace("set calendarTitleExpression (s13:s224) if it exists and is not disabled");
		text("s13:s224", "Cras eget ligula diam. Ut nec bibendum elit, vitae malesuada massa.");
		trace("set calendarStartTime (s13:s232) if it exists and is not disabled");
		_input("s13:s232", "15-Dec-2020 23:19");
		trace("set calendarEndTime (s13:s240) if it exists and is not disabled");
		_input("s13:s240", "15-Dec-2020 23:19");
		trace("set calendarDescriptionExpression (s13:s248) if it exists and is not disabled");
		text("s13:s248", "Aliquam quis lorem in est dignissim semper in vitae mauris. Curabitur id pretium nulla. Integer pulvinar nunc dui, at blandit eros ultricies eget. Cras venenatis sit amet urna a eleifend. In sit amet mi eget libero varius pharetra sit amet quis dolor. Aenean ut odio hendrerit, consequat orci a, ultrices massa. Vestibulum et ex et ligula tincidunt pharetra.  Etiam luctus cursus fermentum.");
		trace("click tab [Manage]");
		tab("s13:s254");
		trace("set moduleName (s13:s261) if it exists and is not disabled");
		selectOne("s13:s261", 2);
		trace("set documentName (s13:s270) if it exists and is not disabled");
		selectOne("s13:s270", 2);
		trace("set tag (s13:s285) if it exists and is not disabled");
		selectOne("s13:s285", 0);
		trace("set unTagSuccessful (s13:s294) if it exists and is not disabled");
		checkbox("s13:s294", Boolean.FALSE);
		trace("set notification (s13:s302) if it exists and is not disabled");
		checkbox("s13:s302", Boolean.FALSE);
		trace("set results (s13:s344) if it exists and is not disabled");
		text("s13:s344", "In sit amet mi eget libero varius pharetra sit amet quis dolor.  In non pretium erat, at imperdiet neque. Duis nec tincidunt velit, id vestibulum sem. Integer pharetra eget erat non ornare. Vivamus maximus tellus neque, nec vestibulum urna gravida et. In hac habitasse platea dictumst. Suspendisse ut mi felis. Phasellus at vehicula tortor. Quisque lacus purus, suscipit et elit eget, interdum semper nibh. Donec eget elit neque. Nullam dignissim ac libero vitae aliquam. Mauris sagittis.  Etiam luctus cursus fermentum. Vivamus ac libero et diam consectetur gravida non vitae nisl. In cursus dignissim est ut varius. Donec dui nibh, vehicula id scelerisque et, convallis eget nibh. Pellentesque pellentesque, lectus at mattis egestas, metus nunc rutrum ipsum, ut imperdiet mi felis sed urna. Sed quis odio quam. Mauris posuere erat at velit convallis, ut sodales lacus tristique. Phasellus dapibus mauris in mattis ultrices. Curabitur eget orci a mi consequat sodales. Curabitur varius quis quam a placerat. Sed in nulla id nunc consectetur elementum ut non magna. Sed ante est, rutrum vel diam id, vehicula euismod lorem. Nullam ullamcorper leo laoreet suscipit scelerisque. Fusce posuere laoreet tempus. Curabitur varius ante quis arcu dignissim, ac elementum nunc pharetra. Vestibulum vitae diam vel quam tempus suscipit vel at arcu. Curabitur id pretium nulla. Proin nec pharetra orci. Sed in ultrices turpis. Cras venenatis sit amet urna a eleifend. Nulla aliquam bibendum massa ut tristique. Integer pulvinar nunc dui, at blandit eros ultricies eget. Vestibulum et ex et ligula tincidunt pharetra. Vivamus congue libero risus, nec cursus tortor congue eget.");
		trace("click tab [Subscriptions]");
		tab("s13:s365");
		trace("click [save] (s376) if it exists and is not disabled");
		button("s376", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Contents]");
		tab("s13:s15");
		trace("click [AddUnsubscribeLink] (s13:s142) if it exists and is not disabled");
		button("s13:s142", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [AddImage] (s13:s146) if it exists and is not disabled");
		button("s13:s146", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Attachments]");
		tab("s13:s154");
		trace("click tab [Manage]");
		tab("s13:s254");
		trace("click [GetCount] (s13:s312) if it exists and is not disabled");
		button("s13:s312", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [TestSend] (s13:s318) if it exists and is not disabled");
		button("s13:s318", true, true);
		trace("Test Success");
		verifySuccess();
		trace("click [GetResults] (s13:s324) if it exists and is not disabled");
		button("s13:s324", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [CreateFiles] (s13:s330) if it exists and is not disabled");
		button("s13:s330", true, true);
		trace("Test Success");
		verifySuccess();
		trace("click [SendNow] (s13:s336) if it exists and is not disabled");
		button("s13:s336", true, true);
		trace("Test Success");
		verifySuccess();
		trace("click [ZipBatch] (s13:s357) if it exists and is not disabled");
		button("s13:s357", true, true);
		trace("Test Success");
		verifySuccess();
		trace("click [DeleteBatch] (s13:s361) if it exists and is not disabled");
		button("s13:s361", true, true);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Subscriptions]");
		tab("s13:s365");
		trace("click [delete] (s377) if it exists and is not disabled");
		redirectButton("s377", true);
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
		trace("click tab [Group Details]");
		tab("s3:s5");
		trace("set name (s3:s13) if it exists and is not disabled");
		text("s3:s13", "Donec dui");
		trace("set description (s3:s21) if it exists and is not disabled");
		text("s3:s21", "Sed ante est, rutrum vel diam id, vehicula euismod lorem. Sed quis odio quam. Vestibulum cursus sollicitudin egestas.Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean ut odio hendrerit, consequat orci a, ultrices massa.");
		trace("click tab [Users in Group]");
		tab("s3:s30");
		trace("click tab [Group Details]");
		tab("s3:s5");
		trace("click tab [Users in Group]");
		tab("s3:s30");
		trace("click [save] (s41) if it exists and is not disabled");
		button("s41", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Group Details]");
		tab("s3:s5");
		trace("set name (s3:s13) if it exists and is not disabled");
		text("s3:s13", "Curabitur id pretium null");
		trace("set description (s3:s21) if it exists and is not disabled");
		text("s3:s21", "Vestibulum et ex et ligula tincidunt pharetra. Vivamus ac libero et diam consectetur gravida non vitae nisl. Phasellus facilisis, nibh eu tincidunt porttitor, magna diam aliquam magna, ut tincidunt erat magna pharetra est.");
		trace("click tab [Users in Group]");
		tab("s3:s30");
		trace("click [save] (s41) if it exists and is not disabled");
		button("s41", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Group Details]");
		tab("s3:s5");
		trace("click tab [Users in Group]");
		tab("s3:s30");
		trace("click [delete] (s42) if it exists and is not disabled");
		redirectButton("s42", true);
		trace("Test Success");
		verifySuccess();
	}

	/**
	 * Menu Security Admin::Data Groups
	 */
	protected void testMenuSecurityAdminDataGroups() throws Exception {
		trace("List for default query of [admin.DataGroup]");
		get("?a=l&m=admin&q=DataGroup");
		trace("set name (s8) if it exists and is not disabled");
		text("s8", "Curabitur");
		trace("set description (s16) if it exists and is not disabled");
		text("s16", "QKHROKXJMWIUULFBQPVSLSGTOVMNVTGUNUSCPJFRZTCWSSSVHOITDCZOQNDZBTDYDENNVPAYZZINCUCJRTIAYEKNRDAUJXIYGQKZOEWEIKCGGZRJMWFVPOVDGUOOMSJWUZPTVXXNOKJHTQZPAFJTIXQQUHVECFUXEPGOWZKKQDEVKBWCIVVDSPVKGFXFXOIXNVPERTIGUWCMLFQVUCKMGXVMGQNPAJZZISWAACEYVPWGZAOOTIXQJYKNITRPOTYMIQSCJMKFJCKJVUOWOUNLVOBYLBZUJCDFOMXLPTLNSUYTEVEJUWUPCQNHVMRSERNXTBEIGBTXPNMICMWMUPKTFLNXDYLIYVJUZNGFTUJLNULEUDIDGHOVRRWRNFZDIHUSLJKPKBKLZBSDLXIVMOKSYAQJIWHCSOTFQJYGUICQKOSNBCTYGOWZQXVXWNXGCAZZSBCULGUTTZXHSFPLQHOEPNBXLKJJLPKQAGFGRUSFZVMDACPOONEWTXQUSDETLLTNBVCFUNACIOFHUNXXVPSRJQTTDVVKDHKCMYOZMKJNYRTYCPCTJSQQRMDDWDYQZGKEPFMYKNOBXABUGZJTSKKVNJWERBELLZYVWLTOCGHNJNBUPWZOSFFFKSDVKLSRGUTDTJSBDPNAACMEVNYANOAQWUYVLHLCMNEOZUUVOWTCOUGTIVENXFQKUPGFNSJSLAEFSYLWLLRKYBTMZKKLRLQQKUYXRVZPCJOTFYCMVBDHHQRHZRABIAOICWWMSDISSESDWUTIUZHXLHUZJHLPGZKHVWCMRWUDPCMYAJLMZSANOYFQWXHQSULQOVKSQDOJBRCTNVDEEZLWBFXEKXPMNDEFBZEVSJFDBIRYJAZHDQCAWWFCGWDESFQSWYFHKGMDNURLIQLJUDVKUYAKWUKMNAVNSGHOEPHLELYEKZGARDJACSQEXTHGPINDTYMIFUCQULPHVNYFKMWXQEPWIALWOJDYQTCEKENTKKMYXIHAWCZZTTZDWCUIQKHVKGOPWVAXXBTJXALSZMTBZZSZTFEZSQXKQIUADSCSWOQTXVRVMLJAOYKZQXJEPOHIDASEHTNPPISCLWFWERLJIMBTXCFOKKXYRKBUWOWKPHBNAUKSEEHABVQWZDGBKSUDXVJMCCLKXXZMWRBXWKUHSQAWCJFXYMYISPFTQSGLFZUVGXCSGAWBDBEMHRFARGEZSLXPCLJHSHXJOSKRSRJNDERENNWKAPZCATDZKNLJMRFKGUYEWGPRMNGCFATEBVXDGPZTVDQREMQUKWESRATKJEVWWMCNGBZZVMJTQMKYSXBCBBSVWMGYDUSCDBXAYEONLUIGAAGYLZIDLDBUOBJBFBAUWPZHRQDBWCCQWAVHKLOMOZXECJECDFHCTXMSAWWGXDWCHHBJZHYOGPIZKMQTEDPPUFSOKKTETKHZCFDJSBIWZDOTDOCNWJNGMSZHPBTSNYTBNDTTIXOFOEXBJSXCZOTGYPVDZKJDUINCLRSBBLOSMKPIPFGDSDRPGGPYKKIZXVKZMTUOJCENWJNSUTNKCDLFIRAFBMTBDXGNRISQOTWZPQNIVCLUPJAFSAJLWQBZCBLDGXNCBNVJRCPWCYYAFWRPZTJRVBXOSVRIRAFWPRLFWNSCLCYJLORSARCHWONUYVXOXWHZXCRPKKKZFTPMVAFQTLGERWDAKLVKANENKWCAIDHYTDRBRVQGFCLZBQSCQODBXCGHLXXCQGZGZMDWVTVWPGHSMXBCXKPUMGPINDGDBBGVIYEVYOYBVAYLQXZFHAVWUAQXXUXEUKGAWEFZWWHMFORPMYTSXOUUQXJPKXYZRXKFPVOPKGPNKUBCPSZMMBCOLYPTOECUOCZYJJTVBRIMPFXAMONMZNBLGLMGVBXSWENTJKSKLFOQAGCIMMVRTENQYQJEJZGXNUXGPTVSLUTCONYCPUFAUTDSYUXFPWNPACFFELFYCMADUNKQWOZZENBRWIGANEHSSDPEWDRRLVLXETFHSXCBXCQURNTUXQZIOJEACQIPQMBYLRIBUWOQWQINXFQEXTIW");
		trace("click [save] (s23) if it exists and is not disabled");
		button("s23", true, false);
		trace("Test Success");
		verifySuccess();
		trace("set name (s8) if it exists and is not disabled");
		text("s8", "Nullam ullamco");
		trace("set description (s16) if it exists and is not disabled");
		text("s16", "Mauris sagittis. In cursus dignissim est ut varius. Sed in nulla id nunc consectetur elementum ut non magna.Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut nec bibendum elit, vitae malesuada massa. Curabitur varius ante quis arcu dignissim, ac elementum nunc pharetra. Proin nec pharetra orci. Phasellus dapibus mauris in mattis ultrices. Sed ante est, rutrum vel diam id, vehicula euismod lorem. Vivamus porttitor, metus vitae lobortis mattis, libero metus tincidunt velit, nec dapibus ipsum arcu vel odio. Vivamus congue libero risus, nec cursus tortor congue eget. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Vestibulum cursus sollicitudin egestas. Vivamus maximus tellus neque, nec vestibulum urna gravida et. Etiam feugiat ipsum sed tellus vulputate congue. Vestibulum et ex et ligula tincidunt pharetra. Donec eget elit neque. Nulla finibus feugiat sapien, at tincidunt nisi pharetra vel.  In non pretium erat, at imperdiet neque. Sed quis odio quam. Duis nec tincidunt velit, id vestibulum sem. In id est at arcu aliquam venenatis. Vestibulum vitae diam vel quam tempus suscipit vel at arcu. Donec sed feugiat neque. Suspendisse ut mi felis.  Nunc facilisis velit ut tempor vestibulum.");
		trace("click [save] (s23) if it exists and is not disabled");
		button("s23", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [delete] (s24) if it exists and is not disabled");
		redirectButton("s24", true);
		trace("Test Success");
		verifySuccess();
	}

	/**
	 * Menu DevOps::Data Maintenance
	 */
	protected void testMenuDevOpsDataMaintenance() throws Exception {
		trace("Edit new document [admin.DataMaintenance] instance");
		get("?a=e&m=admin&d=DataMaintenance");
		trace("click tab [Import/Export]");
		tab("s3:s5");
		trace("click tab [Backup/Restore]");
		tab("s3:s22");
		trace("set dailyBackupRetention (s3:s29) if it exists and is not disabled");
		_input("s3:s29", "135");
		trace("set weeklyBackupRetention (s3:s35) if it exists and is not disabled");
		_input("s3:s35", "5883");
		trace("set monthlyBackupRetention (s3:s41) if it exists and is not disabled");
		_input("s3:s41", "3718");
		trace("set yearlyBackupRetention (s3:s47) if it exists and is not disabled");
		_input("s3:s47", "4698");
		trace("set contentRestoreOption (s3:s92) if it exists and is not disabled");
		radio("s3:s92", 3);
		trace("set restoreIndexingOption (s3:s101) if it exists and is not disabled");
		radio("s3:s101", 4);
		trace("set restorePreProcess (s3:s110) if it exists and is not disabled");
		selectOne("s3:s110", 1);
		trace("click tab [Data Refresh]");
		tab("s3:s137");
		trace("set refreshOption (s3:s160) if it exists and is not disabled");
		selectOne("s3:s160", 1);
		trace("set evictOption (s3:s167) if it exists and is not disabled");
		selectOne("s3:s167", 1);
		trace("set notification (s3:s174) if it exists and is not disabled");
		checkbox("s3:s174", Boolean.FALSE);
		trace("click tab [Import/Export]");
		tab("s3:s5");
		trace("click tab [Backup/Restore]");
		tab("s3:s22");
		trace("click tab [Data Refresh]");
		tab("s3:s137");
		trace("click [save] (s185) if it exists and is not disabled");
		button("s185", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Import/Export]");
		tab("s3:s5");
		trace("click tab [Backup/Restore]");
		tab("s3:s22");
		trace("set dailyBackupRetention (s3:s29) if it exists and is not disabled");
		_input("s3:s29", "7896");
		trace("set weeklyBackupRetention (s3:s35) if it exists and is not disabled");
		_input("s3:s35", "3342");
		trace("set monthlyBackupRetention (s3:s41) if it exists and is not disabled");
		_input("s3:s41", "7009");
		trace("set yearlyBackupRetention (s3:s47) if it exists and is not disabled");
		_input("s3:s47", "1149");
		trace("set contentRestoreOption (s3:s92) if it exists and is not disabled");
		radio("s3:s92", 1);
		trace("set restoreIndexingOption (s3:s101) if it exists and is not disabled");
		radio("s3:s101", 1);
		trace("set restorePreProcess (s3:s110) if it exists and is not disabled");
		selectOne("s3:s110", 3);
		trace("click tab [Data Refresh]");
		tab("s3:s137");
		trace("set refreshOption (s3:s160) if it exists and is not disabled");
		selectOne("s3:s160", 2);
		trace("set evictOption (s3:s167) if it exists and is not disabled");
		selectOne("s3:s167", 2);
		trace("set notification (s3:s174) if it exists and is not disabled");
		checkbox("s3:s174", Boolean.FALSE);
		trace("click [save] (s185) if it exists and is not disabled");
		button("s185", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Import/Export]");
		tab("s3:s5");
		trace("click tab [Backup/Restore]");
		tab("s3:s22");
		trace("click [RefreshBackupList] (s3:s62) if it exists and is not disabled");
		button("s3:s62", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [Backup] (s3:s66) if it exists and is not disabled");
		button("s3:s66", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [DownloadBackup] (s3:s70) if it exists and is not disabled");
		button("s3:s70", true, true);
		trace("Test Success");
		verifySuccess();
		trace("click [UploadBackup] (s3:s74) if it exists and is not disabled");
		button("s3:s74", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [DeleteBackup] (s3:s82) if it exists and is not disabled");
		button("s3:s82", true, true);
		trace("Test Success");
		verifySuccess();
		trace("click [Restore] (s3:s123) if it exists and is not disabled");
		button("s3:s123", true, true);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Data Refresh]");
		tab("s3:s137");
		trace("click [RefreshDocumentTuples] (s3:s180) if it exists and is not disabled");
		button("s3:s180", true, true);
		trace("Test Success");
		verifySuccess();
		trace("click [save] (s185) if it exists and is not disabled");
		button("s185", true, false);
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
		trace("set script (s3:s22) if it exists and is not disabled");
		text("s3:s22", "Maecenas vestibulum lacus id tellus eleifend, vel pellentesque ante lobortis. Nulla finibus feugiat sapien, at tincidunt nisi pharetra vel. Donec eget elit neque. Ut nec bibendum elit, vitae malesuada massa. Sed in nulla id nunc consectetur elementum ut non magna. Vivamus maximus tellus neque, nec vestibulum urna gravida et. Phasellus at vehicula tortor. Curabitur ultrices fermentum lectus a luctus. Vivamus vel neque pellentesque purus hendrerit finibus quis in magna.  In non pretium erat, at imperdiet neque. Praesent ut dignissim ligula. Vestibulum vitae diam vel quam tempus suscipit vel at arcu. Integer pharetra eget erat non ornare. Curabitur id pretium nulla. Vivamus porttitor, metus vitae lobortis mattis, libero metus tincidunt velit, nec dapibus ipsum arcu vel odio.");
		trace("set outputLocation (s3:s30) if it exists and is not disabled");
		text("s3:s30", "Ut nec bibendum elit, vitae malesuada massa. Vestibulum tincidunt placerat euismod. Curabitur id pretium nulla. Vivamus congue libero risus, nec cursus tortor congue eget.");
		trace("click tab [Markdown Preview]");
		tab("s3:s46");
		trace("click tab [Document Preview]");
		tab("s3:s64");
		trace("click tab [Help]");
		tab("s3:s73");
		trace("click tab [Input]");
		tab("s3:s5");
		trace("click tab [Markdown Preview]");
		tab("s3:s46");
		trace("click tab [Document Preview]");
		tab("s3:s64");
		trace("click tab [Help]");
		tab("s3:s73");
		trace("click tab [Input]");
		tab("s3:s5");
		trace("click tab [Markdown Preview]");
		tab("s3:s46");
		trace("click tab [Document Preview]");
		tab("s3:s64");
		trace("click tab [Help]");
		tab("s3:s73");
		trace("click [Submit] (s85) if it exists and is not disabled");
		button("s85", true, true);
		trace("Test Success");
		verifySuccess();
	}

	/**
	 * Menu Snapshots
	 */
	protected void testMenuSnapshots() throws Exception {
		trace("List for default query of [admin.Snapshot]");
		get("?a=l&m=admin&q=Snapshot");
		trace("set moduleName (s8) if it exists and is not disabled");
		selectOne("s8", 1);
		trace("set queryName (s17) if it exists and is not disabled");
		selectOne("s17", 7);
		trace("set name (s26) if it exists and is not disabled");
		text("s26", "Nullam dignissim ac libero vitae aliquam. Vivamus ac libero et diam consectetur gravida non vitae nisl. Nulla risus libero, pharetra ut nisi luctus, commodo vestibulum velit. Aliquam quis lorem in est dignissim semper in vitae mauris.");
		trace("set snapshot (s34) if it exists and is not disabled");
		text("s34", "Nullam ullamcorper leo laoreet suscipit scelerisque. Vivamus porttitor, metus vitae lobortis mattis, libero metus tincidunt velit, nec dapibus ipsum arcu vel odio. Nullam dignissim ac libero vitae aliquam. Duis eget quam vehicula, hendrerit leo ac, ullamcorper augue. In placerat interdum vulputate. Phasellus dapibus mauris in mattis ultrices. Cras venenatis sit amet urna a eleifend.  Aliquam mattis posuere imperdiet. Vestibulum et ex et ligula tincidunt pharetra. Nulla finibus feugiat sapien, at tincidunt nisi pharetra vel.  Nunc facilisis velit ut tempor vestibulum. Curabitur aliquam accumsan purus, sed ultricies elit pulvinar in. Aenean id orci sagittis odio vehicula mollis vel sed risus. Aenean ut odio hendrerit, consequat orci a, ultrices massa. Donec sed feugiat neque.  Cras eget ligula diam. Sed quis odio quam. Vivamus vel neque pellentesque purus hendrerit finibus quis in magna. Etiam feugiat ipsum sed tellus vulputate congue. Ut nec bibendum elit, vitae malesuada massa.Lorem ipsum dolor sit amet, consectetur adipiscing elit. In sit amet mi eget libero varius pharetra sit amet quis dolor. Vivamus maximus tellus neque, nec vestibulum urna gravida et.  In non pretium erat, at imperdiet neque. Suspendisse potenti. Vestibulum tincidunt placerat euismod. Suspendisse varius sit amet lorem vitae efficitur. In id est at arcu aliquam venenatis. Sed ante est, rutrum vel diam id, vehicula euismod lorem. Praesent ut dignissim ligula.");
		trace("click [save] (s62) if it exists and is not disabled");
		button("s62", true, false);
		trace("Test Success");
		verifySuccess();
		trace("set moduleName (s8) if it exists and is not disabled");
		selectOne("s8", 1);
		trace("set queryName (s17) if it exists and is not disabled");
		selectOne("s17", 17);
		trace("set name (s26) if it exists and is not disabled");
		text("s26", "Phasellus facilisis, nibh eu tincidunt porttitor, magna diam aliquam magna, ut tincidunt erat magna pharetra est.");
		trace("set snapshot (s34) if it exists and is not disabled");
		text("s34", "Sed in nulla id nunc consectetur elementum ut non magna. Praesent ut dignissim ligula. In hac habitasse platea dictumst. Donec convallis vitae leo eu hendrerit. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Phasellus dapibus mauris in mattis ultrices. In cursus dignissim est ut varius. Duis nec tincidunt velit, id vestibulum sem. Curabitur id pretium nulla. Suspendisse potenti.  Nunc facilisis velit ut tempor vestibulum. Aenean id orci sagittis odio vehicula mollis vel sed risus. Curabitur eget orci a mi consequat sodales. Donec eget elit neque. Vestibulum cursus sollicitudin egestas. Suspendisse varius sit amet lorem vitae efficitur. Nullam ullamcorper leo laoreet suscipit scelerisque. Duis eget quam vehicula, hendrerit leo ac, ullamcorper augue. Donec dui nibh, vehicula id scelerisque et, convallis eget nibh. Vestibulum et ex et ligula tincidunt pharetra. Donec sed feugiat neque. Pellentesque pellentesque, lectus at mattis egestas, metus nunc rutrum ipsum, ut imperdiet mi felis sed urna. Mauris posuere erat at velit convallis, ut sodales lacus tristique. Vivamus ac libero et diam consectetur gravida non vitae nisl. Quisque lacus purus, suscipit et elit eget, interdum semper nibh. Aenean ut odio hendrerit, consequat orci a, ultrices massa. Mauris sagittis. Ut nec bibendum elit, vitae malesuada massa. Suspendisse ut mi felis. Integer pulvinar nunc dui, at blandit eros ultricies eget. Sed quis odio quam. Integer pharetra eget erat non ornare. Aliquam quis lorem in est dignissim semper in vitae mauris. Nulla finibus feugiat sapien, at tincidunt nisi pharetra vel. In id est at arcu aliquam venenatis. Curabitur aliquam accumsan purus, sed ultricies elit pulvinar in. Vivamus vel neque pellentesque purus hendrerit finibus quis in magna.  Aliquam mattis posuere imperdiet.");
		trace("click [save] (s62) if it exists and is not disabled");
		button("s62", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [CopySnapshotToUser] (s57) if it exists and is not disabled");
		button("s57", true, true);
		trace("Test Success");
		verifySuccess();
		trace("click [delete] (s63) if it exists and is not disabled");
		redirectButton("s63", true);
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
		trace("set moduleName (s8) if it exists and is not disabled");
		text("s8", "Nunc facilisis velit ut tempor vestibulum. Mauris posuere erat at velit convallis, ut sodales lacus tristique. Suspendisse potenti. Nulla aliquam bibendum massa ut tristique. Fusce posuere laoreet tempus.");
		trace("set documentName (s16) if it exists and is not disabled");
		text("s16", "Vivamus ac libero et diam consectetur gravida non vitae nisl. Aliquam quis lorem in est dignissim semper in vitae mauris. Sed in nulla id nunc consectetur elementum ut non magna. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Suspendisse ut mi felis.");
		trace("set sequenceName (s24) if it exists and is not disabled");
		text("s24", "In non pretium erat, at imperdiet neque. Integer pulvinar nunc dui, at blandit eros ultricies eget. Praesent ut dignissim ligula. Sed ante est, rutrum vel diam id, vehicula euismod lorem. Aliquam quis lorem in est dignissim semper in vitae mauris. In hac habitasse platea dictumst.");
		trace("set documentNumber (s32) if it exists and is not disabled");
		text("s32", "Donec convallis vitae leo eu hendrerit. Vestibulum tincidunt placerat euismod. Suspendisse potenti. Curabitur varius quis quam a placerat. Vestibulum cursus sollicitudin egestas.");
		trace("click [save] (s39) if it exists and is not disabled");
		button("s39", true, false);
		trace("Test Success");
		verifySuccess();
		trace("set moduleName (s8) if it exists and is not disabled");
		text("s8", "In placerat interdum vulputate. Nulla risus libero, pharetra ut nisi luctus, commodo vestibulum velit. Curabitur varius quis quam a placerat. Nullam ullamcorper leo laoreet suscipit scelerisque.  Aliquam mattis posuere imperdiet. Vivamus porttitor, metus vitae lobortis mattis, libero metus tincidunt velit, nec dapibus ipsum arcu vel odio.");
		trace("set documentName (s16) if it exists and is not disabled");
		text("s16", "Suspendisse varius sit amet lorem vitae efficitur.  Etiam luctus cursus fermentum. Nulla aliquam bibendum massa ut tristique. Fusce posuere laoreet tempus.  Nunc facilisis velit ut tempor vestibulum.");
		trace("set sequenceName (s24) if it exists and is not disabled");
		text("s24", "XEHVCOMWXADQXEABETMRBURPGQXEPMZCQMJZMKOJAMNEIWEUGWNPUOONNRVGCDKHEAYIYUVJNAREXMIXQAAWBYGYQVGIBQOXEASAUUYMHTEGHLSRKCOSWTPANVOROQFHYGUYVOGEJINCWVACFUKNPYUSXEQEWNFYHGIUHCDKBWEVZTQGKHNOEMZTEDXVXNAWVGAEHDZRDDVDGVNTGZAOPMCQFERUQNQINXDBCMWDPDWVCWFRWOKEPMXATNSHSWOPMMNMFHTCGVYKRBZKUJSVJYHJOBDDOMMHVMFKFVBVBCBMAZIZIUCRYHVSMKLIPKOQAZSCFPFKFWTJGKXUUFNCOSPTAIUXQGLDNWMKDFSBWVPIIWFBGZAAFUDIXHTDSFLRFUJLVECNVYURVTCDKTOBRUZUCHASYEUJUTSPWJOUTPHHZNDRQYBGSSXBADNRWEIESQHTXEQFLTXLGIJCLSQNIHBOYOUZVUIOXHZOHLYLILSSQRJNBUFH");
		trace("set documentNumber (s32) if it exists and is not disabled");
		text("s32", "Pellentesque pellentesque, lectus at mattis egestas, metus nunc rutrum ipsum, ut imperdiet mi felis sed urna. Praesent ut dignissim ligula. Sed ante est, rutrum vel diam id, vehicula euismod lorem.");
		trace("click [save] (s39) if it exists and is not disabled");
		button("s39", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [delete] (s40) if it exists and is not disabled");
		redirectButton("s40", true);
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
		trace("click [Refresh] (s49) if it exists and is not disabled");
		button("s49", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click [DeleteCompletedJobs] (s50) if it exists and is not disabled");
		button("s50", true, true);
		trace("Test Success");
		verifySuccess();
	}

	/**
	 * Test Harness
	 */
	@Test
	public void test() throws Exception {
		testLogin();
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