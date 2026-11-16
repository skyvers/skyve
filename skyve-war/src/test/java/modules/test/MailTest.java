package modules.test;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.skyve.EXT;
import org.skyve.impl.mail.MailServiceStaticSingleton;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Mail;

@SuppressWarnings("deprecation")
class MailTest {
	public static final String EMAIL1 = "email1@skyve.org";
	public static final String EMAIL2 = "email2@skyve.org";
	public static final String EMAIL3 = "email3@skyve.org";
	public static final String EMAIL4 = "email4@skyve.org";
	public static final String EMAIL5 = "email5@skyve.org";
	public static final String EMAIL6 = "email6@skyve.org";
	public static final String EMAIL7 = "email7@skyve.org";
	public static final String SUBJECT = "SUBJECT";
	public static final String BODY = "BODY";
	public static final String POSTMARK_HEADER_NAME = "X-PM-Message-Stream";
	public static final String POSTMARK_HEADER_VALUE = "outbound";

	private static boolean bogusSend = false;
	private static String testRecipient = null;

	@BeforeAll
	static void beforeClass() {
		MailServiceStaticSingleton.setDefault();
		UtilImpl.SMTP = "localhost";
		UtilImpl.SMTP_PORT = 25;
		bogusSend = UtilImpl.SMTP_TEST_BOGUS_SEND;
		UtilImpl.SMTP_TEST_BOGUS_SEND = false;
		testRecipient = UtilImpl.SMTP_TEST_RECIPIENT;
		UtilImpl.SMTP_TEST_RECIPIENT = null;
		UtilImpl.SMTP_HEADERS = new TreeMap<>();
		// This is the real postmark header just for shits and gigs.
		UtilImpl.SMTP_HEADERS.put("X-PM-Message-Stream", "outbound");
	}

	@AfterAll
	static void afterClass() {
		UtilImpl.SMTP_TEST_BOGUS_SEND = bogusSend;
		UtilImpl.SMTP_TEST_RECIPIENT = testRecipient;
		UtilImpl.SMTP_HEADERS = null;
	}
	
	@Test
	@SuppressWarnings("static-method")
	void testSimple() throws Exception {
		try (ByteArrayOutputStream baos = new ByteArrayOutputStream(2048)) {
			EXT.writeMail(new Mail().from(EMAIL1).addTo(EMAIL2).subject(SUBJECT).body(BODY), baos);
			String email = baos.toString(StandardCharsets.UTF_8);
			Assertions.assertTrue(email.contains(EMAIL1), "No from");
			Assertions.assertTrue(email.contains(EMAIL2), "No To");
			Assertions.assertTrue(email.contains(SUBJECT), "No Subject");
			Assertions.assertTrue(email.contains(BODY), "No Body");
		}
	}
	
	@Test
	@SuppressWarnings("static-method")
	void testUnsentHeader() throws Exception {
		try (ByteArrayOutputStream baos = new ByteArrayOutputStream(2048)) {
			EXT.writeMail(new Mail().from(EMAIL1).addTo(EMAIL2).subject(SUBJECT).body(BODY).unsent(), baos);
			String email = baos.toString(StandardCharsets.UTF_8);
			Assertions.assertTrue(email.contains("X-Unsent"), "No unsent header");
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void testJSONHeader() throws Exception {
		try (ByteArrayOutputStream baos = new ByteArrayOutputStream(2048)) {
			EXT.writeMail(new Mail().from(EMAIL1).addTo(EMAIL2).subject(SUBJECT).body(BODY), baos);
			String email = baos.toString(StandardCharsets.UTF_8);
			Assertions.assertTrue(email.contains(POSTMARK_HEADER_NAME), "No postmark header");
			Assertions.assertTrue(email.contains(POSTMARK_HEADER_VALUE), "No postmark header");
		}
	}

	@Test
	void testMultiple() throws Exception {
		try (ByteArrayOutputStream baos = new ByteArrayOutputStream(2048)) {
			EXT.writeMail(new Mail().from(EMAIL1)
										.addTo(EMAIL2).addTo(EMAIL3)
										.addCC(EMAIL4).addCC(EMAIL5)
										.addBCC(EMAIL6).addBCC(EMAIL7)
										.subject(SUBJECT).body(BODY), baos);
			String email = baos.toString(StandardCharsets.UTF_8);
			assertRecipients(email);
		}
	}

	@Test
	void testCollections() throws Exception {
		try (ByteArrayOutputStream baos = new ByteArrayOutputStream(2048)) {
			Set<String> to = new TreeSet<>();
			to.add(EMAIL2);
			to.add(EMAIL3);
			Set<String> cc = new TreeSet<>();
			cc.add(EMAIL4);
			cc.add(EMAIL5);
			Set<String> bcc = new TreeSet<>();
			bcc.add(EMAIL6);
			bcc.add(EMAIL7);
			
			EXT.writeMail(new Mail().from(EMAIL1)
										.addTo(to)
										.addCC(cc)
										.addBCC(bcc)
										.subject(SUBJECT).body(BODY), baos);
			String email = baos.toString(StandardCharsets.UTF_8);
			assertRecipients(email);
		}
	}

	@Test
	void testEllipsis() throws Exception {
		try (ByteArrayOutputStream baos = new ByteArrayOutputStream(2048)) {
			EXT.writeMail(new Mail().from(EMAIL1)
										.addTo(EMAIL2, EMAIL3)
										.addCC(EMAIL4, EMAIL5)
										.addBCC(EMAIL6, EMAIL7)
										.subject(SUBJECT).body(BODY), baos);
			String email = baos.toString(StandardCharsets.UTF_8);
			assertRecipients(email);
		}
	}

	@SuppressWarnings("static-method")
	private void assertRecipients(String email) {
		Assertions.assertTrue(email.contains(EMAIL2), "No To");
		Assertions.assertTrue(email.contains(EMAIL3), "No To");
		Assertions.assertTrue(email.contains(EMAIL4), "No CC");
		Assertions.assertTrue(email.contains(EMAIL5), "No CC");
		Assertions.assertTrue(email.contains(EMAIL6), "No BCC");
		Assertions.assertTrue(email.contains(EMAIL7), "No BCC");
	}
}
