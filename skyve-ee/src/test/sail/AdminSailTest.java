package sail;

import org.junit.After;
import org.junit.Before;
import org.openqa.selenium.By;

import util.sail.Devices;
import util.sail.PrimeFacesTest;

public class AdminSailTest extends PrimeFacesTest {
	private String pathToChromeDriver = "/Users/mike/chromedriver";
//	private String pathToFirefoxDriver = "/usr/local/bin/geckodriver";
	
	@Before
	public void setup() throws Exception {
		setupChrome("http://localhost:8080/skyve/", pathToChromeDriver, Devices.ipad.userAgentString);
//		setupFirefox("http://localhost:8080/skyve/", pathToFirefoxDriver, Devices.ipad.userAgentString);
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
		text("s7password", "Aliquam mattis pos");
		trace("set newPassword (s14password) if it exists and is not disabled");
		text("s14password", "In non pretium erat, at i");
		trace("set confirmPassword (s21password) if it exists and is not disabled");
		text("s21password", "Duis nec tincidunt velit, id v");
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
		text("s39", "In id nisl elit. In id est at arcu aliquam venenatis. Phasellus dapibus mauris in mattis ultrices. Suspendisse varius sit amet lorem vitae efficitur. Integer pharetra diam eget felis imperdiet, ut tempus libero porttitor. Vestibulum tincidunt placerat euismod. Nullam ullamcorper leo laoreet suscipit scelerisque.");
		trace("set lastLogin (s46) if it exists and is not disabled");
		text("s46", "08-May-2018 18:58");
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
		text("s9", "Vivamus porttitor, metus vitae lobortis mattis, libero metus tincidunt velit, nec dapibus ipsum arcu vel");
		trace("set contactType (s16) if it exists and is not disabled");
		selectOne("s16", 2);
		trace("set email1 (s24) if it exists and is not disabled");
		text("s24", "YRUTZUUXHZYPDMDPNGZERCYUCKDSTHEXWBDIKYFTNFEDKYOLCAMDOSDMQSUSRSSQMWZOHPVBRGDSXFFQRLRNQOAXUYYNEPVMWATZLQSJPTOMTNNMBDREVTEAPIFMIOXMFTUFQBMWXGJNDVTFQNMFDRMESCGBZTWPLWFMRTKGNVHMHZBHUVNXBGRVWABEWPOUQQWHIBTFRGTRBQNEWMEDLMPKHWLTELUEJOPGRGPUJDVFLVWBDRQUKCHQR@JRRJRJQKTUFWOTKQKSKQPEDCEFPLORVLNKSFZMLTEEVTATSMGDPAYQVEMOEDILPYQAZAXJIPOVYECOBTYKDUWDMJYKGQIXXBFZOHPBSCEJVFCYANFIKIAMGMPMGVRJDJWMDIYNGTDOATFQDLRPOMCFDJTNAJQTLWGQKKWFMOVCXMHMYQTKBHYRLJHVWKVISTRLCDRKWBQOIUSTNPMFQWTDLJUWXAXWGSNITSEFCYURMIKWUOWWQRIIC.TG");
		trace("set mobile (s31) if it exists and is not disabled");
		text("s31", "0496 441 929");
		trace("click [save] (s50) if it exists and is not disabled");
		button("s50", true, false);
		trace("Test Success");
		verifySuccess();
		trace("set name (s9) if it exists and is not disabled");
		text("s9", "Suspendisse potenti. Sed in ultrices turpis. Donec dui nibh, vehicula id scelerisque et, convallis eget nibh. Duis nec tincidunt velit, id vestibulum sem.");
		trace("set contactType (s16) if it exists and is not disabled");
		selectOne("s16", 1);
		trace("set email1 (s24) if it exists and is not disabled");
		text("s24", "CLLYRACIXPZJSNPRXCJGJJYMZIQUHIRUEZCTODTWWLKAHWCHGUWSBTZCJSFPCFRWJAGXGORKFGEDEKLUDVNQLPYTCNXSJWMMBJMIQYMOTMBPKXWRSVZLLCIAJHSWQADUXWYZAWHZSFXKYLRTODOIEBGMYBMIBKGYSZLTUGARVZUYYXFTDCDAHUIPUTJWMYNDZUVXADLFKHJJMFWYDTCQKLQNSNBVRMRAOMXJPKFNWBHPHIQFNRURPOXOR@RSRRNNDMRHPQHJQMQBFBWWZDFFKMYEQZGKMTOCWBHPKWLUZHUCVOZBBLYOPLRJZXTXBFGLJLOCJNWIQFYWRLAGXPCQEXLMEMDJWUCIBCJFEKVOZSZRECEEZSRCOABSLSVUSZLIDVPQKEYQTIAEIRQHQHPZUUHXDSNEHXLDJWEMOVQLXTIQTKOLUBXQHAYNOMPGYFJCCFVVDOXXGXXBFMXGLAQTRWEMXQHNFKNDUJOKJXFUYPZVQOXJU.RR");
		trace("set mobile (s31) if it exists and is not disabled");
		text("s31", "0463 965 946");
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
		text("s3:s11", "Duis eget quam vehicula, hendrerit leo ac, ullamcorper augue. In id est at arcu aliquam venenatis. Vestibulum cursus sollicitudin egestas. Donec sed feugiat neque.");
		trace("set system (s3:s17) if it exists and is not disabled");
		checkbox("s3:s17", Boolean.FALSE);
		trace("set moduleName (s3:s24) if it exists and is not disabled");
		selectOne("s3:s24", 3);
		trace("set documentName (s3:s32) if it exists and is not disabled");
		selectOne("s3:s32", 0);
		trace("set tag (s3:s40) if it exists and is not disabled");
		selectOne("s3:s40", 0);
		trace("set results (s3:s57) if it exists and is not disabled");
		text("s3:s57", "Nulla aliquam bibendum massa ut tristique. Nullam dignissim ac libero vitae aliquam.  Nunc facilisis velit ut tempor vestibulum. Cras venenatis sit amet urna a eleifend. Donec eget elit neque. Aenean id orci sagittis odio vehicula mollis vel sed risus. In cursus dignissim est ut varius. Proin nec pharetra orci. Nullam ullamcorper leo laoreet suscipit scelerisque. Vivamus vel neque pellentesque purus hendrerit finibus quis in magna. In sit amet mi eget libero varius pharetra sit amet quis dolor. In hac habitasse platea dictumst. Vestibulum cursus sollicitudin egestas.  In non pretium erat, at imperdiet neque. Curabitur varius ante quis arcu dignissim, ac elementum nunc pharetra. Vivamus congue libero risus, nec cursus tortor congue eget. In id est at arcu aliquam venenatis. Sed in nulla id nunc consectetur elementum ut non magna. Pellentesque pellentesque, lectus at mattis egestas, metus nunc rutrum ipsum, ut imperdiet mi felis sed urna. Curabitur id pretium nulla. Vestibulum tincidunt placerat euismod. Quisque lacus purus, suscipit et elit eget, interdum semper nibh. Nulla finibus feugiat sapien, at tincidunt nisi pharetra vel. Etiam feugiat ipsum sed tellus vulputate congue. Phasellus dapibus mauris in mattis ultrices. Suspendisse ut mi felis. Ut nec bibendum elit, vitae malesuada massa. Donec convallis vitae leo eu hendrerit. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Integer pharetra diam eget felis imperdiet, ut tempus libero porttitor. Curabitur aliquam accumsan purus, sed ultricies elit pulvinar in. In id nisl elit.");
		trace("click tab [Contents]");
		tab("s3:s84");
		trace("set formatType (s3:s90) if it exists and is not disabled");
		selectOne("s3:s90", 1);
		trace("set sendFrom (s3:s102) if it exists and is not disabled");
		text("s3:s102", "Nulla risus libero, pharetra ut nisi luctus, commodo vestibulum velit. Mauris sagittis. Sed quis odio quam.Lorem ipsum dolor sit amet, consectetur adipiscing elit.");
		trace("set monitorBcc (s3:s108) if it exists and is not disabled");
		checkbox("s3:s108", Boolean.FALSE);
		trace("set sendTo (s3:s124) if it exists and is not disabled");
		text("s3:s124", "VUKZLNKSDWGWSJHCNSWOHKZVLYFIJIOUPETCHLEMRUODDGGSJXODLMSVOLZGPGAEASZAAEXMNUDKHFQEMGAVIWAXHIGHGHFZJGVIOSBOVZCNFXGAKCFGWRJASNAPBQLLNZEDDTQAVWHAIGKHRLKWNHWDSRXKUAMRNPZDTFMXBHBJULZZEDQMMQMFXNRWFHRKNZOJYBXYCEGDXYWJJXPVCKDMCHUZLBRMWBZVUHODMWJUZSPDGWPQMYEFRR");
		trace("set ccTo (s3:s131) if it exists and is not disabled");
		text("s3:s131", "Maecenas vestibulum lacus id tellus eleifend, vel pellentesque ante lobortis. Vivamus vel neque pellentesque purus hendrerit finibus quis in magna. Phasellus dapibus mauris in mattis ultrices.");
		trace("set subject (s3:s147) if it exists and is not disabled");
		text("s3:s147", "Curabitur varius quis quam a placerat. Vestibulum vitae diam vel quam tempus suscipit vel at arcu. In hac habitasse platea dictumst.");
		trace("set includeCalendar (s3:s173) if it exists and is not disabled");
		checkbox("s3:s173", Boolean.FALSE);
		trace("click tab [Options]");
		tab("s3:s187");
		trace("set attachmentFileName1 (s3:s205) if it exists and is not disabled");
		text("s3:s205", "Proin nec pharetra orci. Duis eget quam vehicula, hendrerit leo ac, ullamcorper augue. Ut nec bibendum elit, vitae malesuada massa.");
		trace("set attachmentFileName2 (s3:s224) if it exists and is not disabled");
		text("s3:s224", "Quisque varius, sem eu varius molestie, eros augue cursus purus, nec mattis leo mauris vitae nisi. Donec sed feugiat neque.");
		trace("set attachmentFileName3 (s3:s243) if it exists and is not disabled");
		text("s3:s243", "Etiam feugiat ipsum sed tellus vulputate congue. Duis nec tincidunt velit, id vestibulum sem. Mauris posuere erat at velit convallis, ut sodales lacus tristique.");
		trace("set calendarTitleExpression (s3:s253) if it exists and is not disabled");
		text("s3:s253", "Pellentesque pellentesque, lectus at mattis egestas, metus nunc rutrum ipsum, ut imperdiet mi felis sed urna. Vivamus congue libero risus, nec cursus tortor congue eget.");
		trace("set calendarStartTime (s3:s260) if it exists and is not disabled");
		_input("s3:s260", "");
		trace("set calendarEndTime (s3:s267) if it exists and is not disabled");
		_input("s3:s267", "");
		trace("set calendarDescriptionExpression (s3:s274) if it exists and is not disabled");
		text("s3:s274", "Curabitur id pretium nulla.  Nunc facilisis velit ut tempor vestibulum. Donec dui nibh, vehicula id scelerisque et, convallis eget nibh. Vestibulum vitae diam vel quam tempus suscipit vel at arcu.Lorem ipsum dolor sit amet, consectetur adipiscing elit.");
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
		text("s3:s11", "Duis nec tincidunt velit, id vestibulum sem. Integer pharetra diam eget felis imperdiet, ut tempus libero porttitor. Suspendisse ut mi felis. Fusce posuere laoreet tempus.  Etiam luctus cursus fermentum.");
		trace("set system (s3:s17) if it exists and is not disabled");
		checkbox("s3:s17", Boolean.FALSE);
		trace("set moduleName (s3:s24) if it exists and is not disabled");
		selectOne("s3:s24", 2);
		trace("set documentName (s3:s32) if it exists and is not disabled");
		selectOne("s3:s32", 0);
		trace("set tag (s3:s40) if it exists and is not disabled");
		selectOne("s3:s40", 0);
		trace("set results (s3:s57) if it exists and is not disabled");
		text("s3:s57", "Nulla finibus feugiat sapien, at tincidunt nisi pharetra vel. Curabitur eget orci a mi consequat sodales. Curabitur ultrices fermentum lectus a luctus. Quisque lacus purus, suscipit et elit eget, interdum semper nibh. Phasellus at vehicula tortor. Nulla aliquam bibendum massa ut tristique. In cursus dignissim est ut varius. Curabitur id pretium nulla. Duis eget quam vehicula, hendrerit leo ac, ullamcorper augue. Vestibulum et ex et ligula tincidunt pharetra. Mauris posuere erat at velit convallis, ut sodales lacus tristique. Vivamus ac libero et diam consectetur gravida non vitae nisl. Vivamus vel neque pellentesque purus hendrerit finibus quis in magna. Fusce posuere laoreet tempus. Integer pharetra eget erat non ornare. In id est at arcu aliquam venenatis. Etiam feugiat ipsum sed tellus vulputate congue. Proin nec pharetra orci. In id nisl elit.  In non pretium erat, at imperdiet neque. Donec eget elit neque.  Etiam luctus cursus fermentum. In sit amet mi eget libero varius pharetra sit amet quis dolor. Donec dui nibh, vehicula id scelerisque et, convallis eget nibh. Suspendisse ut mi felis.  Nunc facilisis velit ut tempor vestibulum. Pellentesque pellentesque, lectus at mattis egestas, metus nunc rutrum ipsum, ut imperdiet mi felis sed urna. Sed in nulla id nunc consectetur elementum ut non magna. Vestibulum cursus sollicitudin egestas. Curabitur varius ante quis arcu dignissim, ac elementum nunc pharetra.  Cras eget ligula diam. Phasellus dapibus mauris in mattis ultrices. Phasellus facilisis, nibh eu tincidunt porttitor, magna diam aliquam magna, ut tincidunt erat magna pharetra est. Sed in ultrices turpis. Nullam ullamcorper leo laoreet suscipit scelerisque. Curabitur varius quis quam a placerat. Integer pulvinar nunc dui, at blandit eros ultricies eget.");
		trace("click tab [Contents]");
		tab("s3:s84");
		trace("set formatType (s3:s90) if it exists and is not disabled");
		selectOne("s3:s90", 1);
		trace("set sendFrom (s3:s102) if it exists and is not disabled");
		text("s3:s102", "Nunc facilisis velit ut tempor vestibulum. Sed in ultrices turpis. Mauris posuere erat at velit convallis, ut sodales lacus tristique. Ut nec bibendum elit, vitae malesuada massa.");
		trace("set monitorBcc (s3:s108) if it exists and is not disabled");
		checkbox("s3:s108", Boolean.FALSE);
		trace("set sendTo (s3:s124) if it exists and is not disabled");
		text("s3:s124", "Curabitur eget orci a mi consequat sodales. Donec convallis vitae leo eu hendrerit. Nullam ullamcorper leo laoreet suscipit scelerisque.");
		trace("set ccTo (s3:s131) if it exists and is not disabled");
		text("s3:s131", "Donec eget elit neque. Cras venenatis sit amet urna a eleifend. Vestibulum vitae diam vel quam tempus suscipit vel at arcu. Phasellus dapibus mauris in mattis ultrices.");
		trace("set subject (s3:s147) if it exists and is not disabled");
		text("s3:s147", "Nullam ullamcorper leo laoreet suscipit scelerisque. Etiam feugiat ipsum sed tellus vulputate congue. In cursus dignissim est ut varius.");
		trace("set includeCalendar (s3:s173) if it exists and is not disabled");
		checkbox("s3:s173", Boolean.FALSE);
		trace("click tab [Options]");
		tab("s3:s187");
		trace("set attachmentFileName1 (s3:s205) if it exists and is not disabled");
		text("s3:s205", "Cras venenatis sit amet urna a eleifend. Vestibulum vitae diam vel quam tempus suscipit vel at arcu. Curabitur aliquam accumsan purus, sed ultricies elit pulvinar in.");
		trace("set attachmentFileName2 (s3:s224) if it exists and is not disabled");
		text("s3:s224", "Vestibulum et ex et ligula tincidunt pharetra. Vivamus maximus tellus neque, nec vestibulum urna gravida et.  Aliquam mattis posuere imperdiet.");
		trace("set attachmentFileName3 (s3:s243) if it exists and is not disabled");
		text("s3:s243", "Mauris sagittis. Curabitur varius ante quis arcu dignissim, ac elementum nunc pharetra. Etiam feugiat ipsum sed tellus vulputate congue.");
		trace("set calendarTitleExpression (s3:s253) if it exists and is not disabled");
		text("s3:s253", "Vestibulum cursus sollicitudin egestas. Donec dui nibh, vehicula id scelerisque et, convallis eget nibh.  In non pretium erat, at imperdiet neque.");
		trace("set calendarStartTime (s3:s260) if it exists and is not disabled");
		_input("s3:s260", "");
		trace("set calendarEndTime (s3:s267) if it exists and is not disabled");
		_input("s3:s267", "");
		trace("set calendarDescriptionExpression (s3:s274) if it exists and is not disabled");
		text("s3:s274", "Nulla aliquam bibendum massa ut tristique. Donec dui nibh, vehicula id scelerisque et, convallis eget nibh. Suspendisse ut mi felis. Vestibulum tincidunt placerat euismod. Vestibulum cursus sollicitudin egestas. Ut nec bibendum elit, vitae malesuada massa. Aenean id orci sagittis odio vehicula mollis vel sed risus. Fusce posuere laoreet tempus. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Phasellus at vehicula tortor.");
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
		text("s11", "Suspendisse varius sit am");
		trace("set description (s18) if it exists and is not disabled");
		text("s18", "Mauris sagittis. Aliquam quis lorem in est dignissim semper in vitae mauris. Phasellus at vehicula tortor. Vivamus ac libero et diam consectetur gravida non vitae nisl.  Etiam luctus cursus fermentum.");
		trace("New row on data grid [roles] (s24)");
		dataGridButton("s24", "s24:s33", false);
		trace("set roleName (s7) if it exists and is not disabled");
		selectOne("s7", 7);
		trace("click [zoom out] (s17) if it exists and is not disabled");
		button("s17", false, false);
		trace("Test Success");
		verifySuccess();
		trace("click [save] (s45) if it exists and is not disabled");
		button("s45", true, false);
		trace("Test Success");
		verifySuccess();
		trace("set name (s11) if it exists and is not disabled");
		text("s11", "Cras venenatis sit");
		trace("set description (s18) if it exists and is not disabled");
		text("s18", "In non pretium erat, at imperdiet neque. Curabitur ultrices fermentum lectus a luctus. Aenean id orci sagittis odio vehicula mollis vel sed risus.");
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
		text("s7", "Integer pulvinar nun");
		trace("set description (s14) if it exists and is not disabled");
		text("s14", "Donec sed feugiat neque. Nullam dignissim ac libero vitae aliquam. Sed in ultrices turpis. Ut nec bibendum elit, vitae malesuada massa.Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec convallis vitae leo eu hendrerit. In sit amet mi eget libero varius pharetra sit amet quis dolor. Vivamus congue libero risus, nec cursus tortor congue eget. Quisque varius, sem eu varius molestie, eros augue cursus purus, nec mattis leo mauris vitae nisi.  Aliquam mattis posuere imperdiet. In hac habitasse platea dictumst. In cursus dignissim est ut varius. In id nisl elit. Sed quis odio quam. Sed ante est, rutrum vel diam id, vehicula euismod lorem.  In non pretium erat, at imperdiet neque. Quisque lacus purus, suscipit et elit eget, interdum semper nibh. Curabitur ultrices fermentum lectus a luctus. Proin nec pharetra orci. Aenean ut odio hendrerit, consequat orci a, ultrices massa. Cras venenatis sit amet urna a eleifend. Praesent ut dignissim ligula. In id est at arcu aliquam venenatis. Aliquam quis lorem in est dignissim semper in vitae mauris. Mauris posuere erat at velit convallis, ut sodales lacus tristique. Nulla aliquam bibendum massa ut tristique. Phasellus at vehicula tortor. Sed in nulla id nunc consectetur elementum ut non magna. Duis eget quam vehicula, hendrerit leo ac, ullamcorper augue. Curabitur eget orci a mi consequat sodales.  Nunc facilisis velit ut tempor vestibulum. Curabitur id pretium nulla. Integer pharetra diam eget felis imperdiet, ut tempus libero porttitor. Integer pharetra eget erat non ornare. Suspendisse ut mi felis. Integer pulvinar nunc dui, at blandit eros ultricies eget. Vestibulum cursus sollicitudin egestas. Mauris sagittis. Duis nec tincidunt velit, id vestibulum sem. Maecenas vestibulum lacus id tellus eleifend, vel pellentesque ante lobortis. Nulla risus libero, pharetra ut nisi luctus, commodo vestibulum velit.");
		trace("click [save] (s21) if it exists and is not disabled");
		button("s21", true, false);
		trace("Test Success");
		verifySuccess();
		trace("set name (s7) if it exists and is not disabled");
		text("s7", "Vestibulum vitae dia");
		trace("set description (s14) if it exists and is not disabled");
		text("s14", "Curabitur aliquam accumsan purus, sed ultricies elit pulvinar in. Pellentesque pellentesque, lectus at mattis egestas, metus nunc rutrum ipsum, ut imperdiet mi felis sed urna.  In non pretium erat, at imperdiet neque. Vestibulum vitae diam vel quam tempus suscipit vel at arcu. Donec eget elit neque. Aenean ut odio hendrerit, consequat orci a, ultrices massa.  Cras eget ligula diam. In sit amet mi eget libero varius pharetra sit amet quis dolor. Phasellus facilisis, nibh eu tincidunt porttitor, magna diam aliquam magna, ut tincidunt erat magna pharetra est. Vivamus vel neque pellentesque purus hendrerit finibus quis in magna. Sed in ultrices turpis. In cursus dignissim est ut varius. In hac habitasse platea dictumst. In placerat interdum vulputate. Etiam feugiat ipsum sed tellus vulputate congue. Suspendisse varius sit amet lorem vitae efficitur.  Nunc facilisis velit ut tempor vestibulum.  Aliquam mattis posuere imperdiet. Praesent ut dignissim ligula. Nulla finibus feugiat sapien, at tincidunt nisi pharetra vel. Donec convallis vitae leo eu hendrerit. Mauris sagittis. Vivamus ac libero et diam consectetur gravida non vitae nisl. Nullam ullamcorper leo laoreet suscipit scelerisque. Donec sed feugiat neque. Quisque lacus purus, suscipit et elit eget, interdum semper nibh.  Etiam luctus cursus fermentum. Vivamus porttitor, metus vitae lobortis mattis, libero metus tincidunt velit, nec dapibus ipsum arcu vel odio.");
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
		selectOne("s3:s11", 26);
		trace("click tab [Backup/Restore]");
		tab("s3:s36");
		trace("set dailyBackupRetention (s3:s42) if it exists and is not disabled");
		_input("s3:s42", "4194");
		trace("set weeklyBackupRetention (s3:s48) if it exists and is not disabled");
		_input("s3:s48", "64");
		trace("set monthlyBackupRetention (s3:s54) if it exists and is not disabled");
		_input("s3:s54", "4296");
		trace("set yearlyBackupRetention (s3:s60) if it exists and is not disabled");
		_input("s3:s60", "8762");
		trace("set restorePreProcess (s3:s78) if it exists and is not disabled");
		selectOne("s3:s78", 1);
		trace("click tab [Data]");
		tab("s3:s109");
		trace("set schemaName (s3:s115) if it exists and is not disabled");
		text("s3:s115", "Cras eget ligula diam. Quisque varius, sem eu varius molestie, eros augue cursus purus, nec mattis leo mauris vitae nisi. Nullam ullamcorper leo laoreet suscipit scelerisque. Mauris posuere erat at velit convallis, ut sodales lacus tristique. Vestibulum tincidunt placerat euismod. Curabitur varius quis quam a placerat. Integer pulvinar nunc dui, at blandit eros ultricies eget. Vestibulum et ex et ligula tincidunt pharetra. Pellentesque pellentesque, lectus at mattis egestas, metus nunc rutrum ipsum, ut imperdiet mi felis sed urna. Curabitur eget orci a mi consequat sodales. Maecenas vestibulum lacus id tellus eleifend, vel pellentesque ante lobortis. Donec eget elit neque. Donec sed feugiat neque. Integer pharetra eget erat non ornare. Duis eget quam vehicula, hendrerit leo ac, ullamcorper augue.");
		trace("click tab [Content]");
		tab("s3:s144");
		trace("click tab [Manage Audits]");
		tab("s3:s172");
		trace("set auditModuleName (s3:s178) if it exists and is not disabled");
		selectOne("s3:s178", 3);
		trace("set auditDocumentName (s3:s186) if it exists and is not disabled");
		selectOne("s3:s186", 3);
		trace("set auditOperation (s3:s194) if it exists and is not disabled");
		selectOne("s3:s194", 1);
		trace("set auditTimestampStart (s3:s202) if it exists and is not disabled");
		_input("s3:s202", "08-May-2018 18:58:37");
		trace("set auditTimestampEnd (s3:s209) if it exists and is not disabled");
		_input("s3:s209", "08-May-2018 18:58:37");
		trace("set auditMatchCount (s3:s223) if it exists and is not disabled");
		text("s3:s223", "5491");
		trace("click tab [Refresh]");
		tab("s3:s265");
		trace("set refreshOption (s3:s286) if it exists and is not disabled");
		selectOne("s3:s286", 2);
		trace("set notification (s3:s293) if it exists and is not disabled");
		checkbox("s3:s293", Boolean.FALSE);
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
		trace("click [save] (s304) if it exists and is not disabled");
		button("s304", true, false);
		trace("Test Success");
		verifySuccess();
		trace("click tab [Export/Import]");
		tab("s3:s5");
		trace("set modDocName (s3:s11) if it exists and is not disabled");
		selectOne("s3:s11", 4);
		trace("click tab [Backup/Restore]");
		tab("s3:s36");
		trace("set dailyBackupRetention (s3:s42) if it exists and is not disabled");
		_input("s3:s42", "3374");
		trace("set weeklyBackupRetention (s3:s48) if it exists and is not disabled");
		_input("s3:s48", "7198");
		trace("set monthlyBackupRetention (s3:s54) if it exists and is not disabled");
		_input("s3:s54", "8701");
		trace("set yearlyBackupRetention (s3:s60) if it exists and is not disabled");
		_input("s3:s60", "6601");
		trace("set restorePreProcess (s3:s78) if it exists and is not disabled");
		selectOne("s3:s78", 2);
		trace("click tab [Data]");
		tab("s3:s109");
		trace("set schemaName (s3:s115) if it exists and is not disabled");
		text("s3:s115", "Curabitur varius ante quis arcu dignissim, ac elementum nunc pharetra. Quisque lacus purus, suscipit et elit eget, interdum semper nibh. Vivamus porttitor, metus vitae lobortis mattis, libero metus tincidunt velit, nec dapibus ipsum arcu vel odio. Phasellus at vehicula tortor. Integer pharetra diam eget felis imperdiet, ut tempus libero porttitor. In id nisl elit. Aenean id orci sagittis odio vehicula mollis vel sed risus. Duis nec tincidunt velit, id vestibulum sem. Curabitur aliquam accumsan purus, sed ultricies elit pulvinar in. Vivamus maximus tellus neque, nec vestibulum urna gravida et. Curabitur ultrices fermentum lectus a luctus.  In non pretium erat, at imperdiet neque.");
		trace("click tab [Content]");
		tab("s3:s144");
		trace("click tab [Manage Audits]");
		tab("s3:s172");
		trace("set auditModuleName (s3:s178) if it exists and is not disabled");
		selectOne("s3:s178", 1);
		trace("set auditDocumentName (s3:s186) if it exists and is not disabled");
		selectOne("s3:s186", 2);
		trace("set auditOperation (s3:s194) if it exists and is not disabled");
		selectOne("s3:s194", 1);
		trace("set auditTimestampStart (s3:s202) if it exists and is not disabled");
		_input("s3:s202", "08-May-2018 18:58:37");
		trace("set auditTimestampEnd (s3:s209) if it exists and is not disabled");
		_input("s3:s209", "08-May-2018 18:58:37");
		trace("set auditMatchCount (s3:s223) if it exists and is not disabled");
		text("s3:s223", "6472");
		trace("click tab [Refresh]");
		tab("s3:s265");
		trace("set refreshOption (s3:s286) if it exists and is not disabled");
		selectOne("s3:s286", 1);
		trace("set notification (s3:s293) if it exists and is not disabled");
		checkbox("s3:s293", Boolean.FALSE);
		trace("click [save] (s304) if it exists and is not disabled");
		button("s304", true, false);
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
		trace("click [RefreshDocumentTuples] (s3:s299) if it exists and is not disabled");
		button("s3:s299", true, true);
		trace("Test Success");
		verifySuccess();
		trace("click [save] (s304) if it exists and is not disabled");
		button("s304", true, false);
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
		text("s3:s11", "Donec eget elit neque. Donec dui nibh, vehicula id scelerisque et, convallis eget nibh. Nulla finibus feugiat sapien, at tincidunt nisi pharetra vel. Aenean id orci sagittis odio vehicula mollis vel sed risus. Aliquam quis lorem in est dignissim semper in vitae mauris. Pellentesque pellentesque, lectus at mattis egestas, metus nunc rutrum ipsum, ut imperdiet mi felis sed urna. In hac habitasse platea dictumst. Aenean ut odio hendrerit, consequat orci a, ultrices massa.  In non pretium erat, at imperdiet neque.  Aliquam mattis posuere imperdiet. Suspendisse potenti. Nulla aliquam bibendum massa ut tristique. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Curabitur varius quis quam a placerat. In cursus dignissim est ut varius. Cras venenatis sit amet urna a eleifend. Proin nec pharetra orci. Vestibulum cursus sollicitudin egestas. Phasellus at vehicula tortor. Fusce posuere laoreet tempus. Sed in ultrices turpis.");
		trace("set outputLocation (s3:s18) if it exists and is not disabled");
		text("s3:s18", "Sed in ultrices turpis.  Nunc facilisis velit ut tempor vestibulum.");
		trace("set defaultModule (s3:s25) if it exists and is not disabled");
		selectOne("s3:s25", 2);
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
		text("s7", "In id nisl elit. Donec eget elit neque.  Cras eget ligula diam.");
		trace("set queryName (s14) if it exists and is not disabled");
		text("s14", "Lorem ipsum dolor sit amet, consectetur adipiscing elit");
		trace("set name (s21) if it exists and is not disabled");
		text("s21", "In placerat interdum vulputate. Curabitur id pretium nulla.  Cras eget ligula diam. Vestibulum tincidunt placerat euismod. Duis nec tincidunt velit, id vestibulum sem.");
		trace("set snapshot (s28) if it exists and is not disabled");
		text("s28", "In sit amet mi eget libero varius pharetra sit amet quis dolor. Donec eget elit neque. Integer pharetra eget erat non ornare. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. In id nisl elit. Proin nec pharetra orci. Sed ante est, rutrum vel diam id, vehicula euismod lorem. Vestibulum et ex et ligula tincidunt pharetra. Phasellus facilisis, nibh eu tincidunt porttitor, magna diam aliquam magna, ut tincidunt erat magna pharetra est. Nulla finibus feugiat sapien, at tincidunt nisi pharetra vel. Quisque lacus purus, suscipit et elit eget, interdum semper nibh. Donec convallis vitae leo eu hendrerit. Suspendisse varius sit amet lorem vitae efficitur. Vivamus congue libero risus, nec cursus tortor congue eget. Curabitur id pretium nulla. Phasellus at vehicula tortor. Ut nec bibendum elit, vitae malesuada massa. Nulla aliquam bibendum massa ut tristique. Maecenas vestibulum lacus id tellus eleifend, vel pellentesque ante lobortis. Quisque varius, sem eu varius molestie, eros augue cursus purus, nec mattis leo mauris vitae nisi. Curabitur eget orci a mi consequat sodales. In placerat interdum vulputate.  Cras eget ligula diam. Phasellus dapibus mauris in mattis ultrices. Praesent ut dignissim ligula.");
		trace("set copyToUserSnapshotName (s45) if it exists and is not disabled");
		text("s45", "Suspendisse ut mi felis. Donec sed feugiat neque.");
		trace("click [save] (s61) if it exists and is not disabled");
		button("s61", true, false);
		trace("Test Success");
		verifySuccess();
		trace("set moduleName (s7) if it exists and is not disabled");
		text("s7", "Quisque varius, sem eu varius molestie, eros augue");
		trace("set queryName (s14) if it exists and is not disabled");
		text("s14", "Donec eget elit neque. Phasellus at vehicula tortor.");
		trace("set name (s21) if it exists and is not disabled");
		text("s21", "Vestibulum vitae diam vel quam tempus suscipit vel at arcu. In hac habitasse platea dictumst.");
		trace("set snapshot (s28) if it exists and is not disabled");
		text("s28", "In placerat interdum vulputate. Vivamus congue libero risus, nec cursus tortor congue eget. Vestibulum et ex et ligula tincidunt pharetra. Phasellus at vehicula tortor. Sed in nulla id nunc consectetur elementum ut non magna. Nullam ullamcorper leo laoreet suscipit scelerisque. Vivamus maximus tellus neque, nec vestibulum urna gravida et. Maecenas vestibulum lacus id tellus eleifend, vel pellentesque ante lobortis. Ut nec bibendum elit, vitae malesuada massa. In cursus dignissim est ut varius. Aenean id orci sagittis odio vehicula mollis vel sed risus. Curabitur eget orci a mi consequat sodales. Cras venenatis sit amet urna a eleifend. In hac habitasse platea dictumst. Mauris sagittis. Curabitur varius quis quam a placerat.  Nunc facilisis velit ut tempor vestibulum. Donec eget elit neque.");
		trace("set copyToUserSnapshotName (s45) if it exists and is not disabled");
		text("s45", "Mauris sagittis. Proin nec pharetra orci. Curabitur eget orci a mi consequat sodales.");
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
		text("s7", "Pellentesque pellentesque, lectus at mattis egestas, metus nunc rutrum ipsum, ut imperdiet mi felis sed urna. Phasellus facilisis, nibh eu tincidunt porttitor, magna diam aliquam magna, ut tincidunt erat magna pharetra est. Proin nec pharetra orci.");
		trace("set documentName (s14) if it exists and is not disabled");
		text("s14", "Vestibulum et ex et ligula tincidunt pharetra.  Cras eget ligula diam. Pellentesque pellentesque, lectus at mattis egestas, metus nunc rutrum ipsum, ut imperdiet mi felis sed urna. Proin nec pharetra orci. Vestibulum vitae diam vel quam tempus suscipit vel at arcu. Curabitur varius quis quam a placerat. Curabitur id pretium nulla. Nulla risus libero, pharetra ut nisi luctus, commodo vestibulum velit. Phasellus at vehicula tortor.");
		trace("set sequenceName (s21) if it exists and is not disabled");
		text("s21", "In hac habitasse platea dictumst. Maecenas vestibulum lacus id tellus eleifend, vel pellentesque ante lobortis. Cras venenatis sit amet urna a eleifend.  Cras eget ligula diam. Donec convallis vitae leo eu hendrerit. Curabitur id pretium nulla. Ut nec bibendum elit, vitae malesuada massa. Phasellus dapibus mauris in mattis ultrices.");
		trace("set documentNumber (s28) if it exists and is not disabled");
		text("s28", "Phasellus at vehicula tortor. Integer pulvinar nunc dui, at blandit eros ultricies eget. Mauris posuere erat at velit convallis, ut sodales lacus tristique. Mauris sagittis.  Cras eget ligula diam. Etiam feugiat ipsum sed tellus vulputate congue. Vivamus ac libero et diam consectetur gravida non vitae nisl.  Etiam luctus cursus fermentum. Vivamus congue libero risus, nec cursus tortor congue eget. Nulla aliquam bibendum massa ut tristique.");
		trace("click [save] (s35) if it exists and is not disabled");
		button("s35", true, false);
		trace("Test Success");
		verifySuccess();
		trace("set moduleName (s7) if it exists and is not disabled");
		text("s7", "Suspendisse potenti. Vivamus maximus tellus neque, nec vestibulum urna gravida et. Nulla risus libero, pharetra ut nisi luctus, commodo vestibulum velit. Proin nec pharetra orci. Cras venenatis sit amet urna a eleifend. Duis eget quam vehicula, hendrerit leo ac, ullamcorper augue. In placerat interdum vulputate. Vestibulum et ex et ligula tincidunt pharetra. Quisque varius, sem eu varius molestie, eros augue cursus purus, nec mattis leo mauris vitae nisi.");
		trace("set documentName (s14) if it exists and is not disabled");
		text("s14", "Suspendisse ut mi felis. Quisque varius, sem eu varius molestie, eros augue cursus purus, nec mattis leo mauris vitae nisi.");
		trace("set sequenceName (s21) if it exists and is not disabled");
		text("s21", "Etiam luctus cursus fermentum. Phasellus facilisis, nibh eu tincidunt porttitor, magna diam aliquam magna, ut tincidunt erat magna pharetra est. Curabitur ultrices fermentum lectus a luctus.");
		trace("set documentNumber (s28) if it exists and is not disabled");
		text("s28", "Nullam ullamcorper leo laoreet suscipit scelerisque. Donec dui nibh, vehicula id scelerisque et, convallis eget nibh. Curabitur aliquam accumsan purus, sed ultricies elit pulvinar in. In id est at arcu aliquam venenatis. Cras venenatis sit amet urna a eleifend. Vivamus congue libero risus, nec cursus tortor congue eget. Curabitur id pretium nulla. Etiam feugiat ipsum sed tellus vulputate congue. Donec sed feugiat neque.");
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
