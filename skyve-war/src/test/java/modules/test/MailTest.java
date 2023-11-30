package modules.test;

import java.io.ByteArrayOutputStream;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;
import org.skyve.EXT;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Mail;
import org.skyve.util.Util;

import util.AbstractH2Test;

public class MailTest extends AbstractH2Test {
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

	@BeforeClass
	public static void beforeClass() {
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

	@AfterClass
	public static void afterClass() {
		UtilImpl.SMTP_TEST_BOGUS_SEND = bogusSend;
		UtilImpl.SMTP_TEST_RECIPIENT = testRecipient;
		UtilImpl.SMTP_HEADERS = null;
	}
	
	@Test
	@SuppressWarnings("static-method")
	public void testSimple() throws Exception {
		try (ByteArrayOutputStream baos = new ByteArrayOutputStream(2048)) {
			EXT.writeMail(new Mail().from(EMAIL1).addTo(EMAIL2).subject(SUBJECT).body(BODY), baos);
			String email = baos.toString(Util.UTF8);
			Assert.assertTrue("No from", email.contains(EMAIL1));
			Assert.assertTrue("No To", email.contains(EMAIL2));
			Assert.assertTrue("No Subject", email.contains(SUBJECT));
			Assert.assertTrue("No Body", email.contains(BODY));
		}
	}
	
	@Test
	@SuppressWarnings("static-method")
	public void testUnsentHeader() throws Exception {
		try (ByteArrayOutputStream baos = new ByteArrayOutputStream(2048)) {
			EXT.writeMail(new Mail().from(EMAIL1).addTo(EMAIL2).subject(SUBJECT).body(BODY).unsent(), baos);
			String email = baos.toString(Util.UTF8);
			Assert.assertTrue("No unsent header", email.contains("X-Unsent"));
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void testJSONHeader() throws Exception {
		try (ByteArrayOutputStream baos = new ByteArrayOutputStream(2048)) {
			EXT.writeMail(new Mail().from(EMAIL1).addTo(EMAIL2).subject(SUBJECT).body(BODY), baos);
			String email = baos.toString(Util.UTF8);
			Assert.assertTrue("No postmark header", email.contains(POSTMARK_HEADER_NAME));
			Assert.assertTrue("No postmark header", email.contains(POSTMARK_HEADER_VALUE));
		}
	}

	@Test
	public void testMultiple() throws Exception {
		try (ByteArrayOutputStream baos = new ByteArrayOutputStream(2048)) {
			EXT.writeMail(new Mail().from(EMAIL1)
										.addTo(EMAIL2).addTo(EMAIL3)
										.addCC(EMAIL4).addCC(EMAIL5)
										.addBCC(EMAIL6).addBCC(EMAIL7)
										.subject(SUBJECT).body(BODY), baos);
			String email = baos.toString(Util.UTF8);
			assertRecipients(email);
		}
	}

	@Test
	public void testCollections() throws Exception {
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
			String email = baos.toString(Util.UTF8);
			assertRecipients(email);
		}
	}

	@Test
	public void testEllipsis() throws Exception {
		try (ByteArrayOutputStream baos = new ByteArrayOutputStream(2048)) {
			EXT.writeMail(new Mail().from(EMAIL1)
										.addTo(EMAIL2, EMAIL3)
										.addCC(EMAIL4, EMAIL5)
										.addBCC(EMAIL6, EMAIL7)
										.subject(SUBJECT).body(BODY), baos);
			String email = baos.toString(Util.UTF8);
			assertRecipients(email);
		}
	}

	@SuppressWarnings("static-method")
	private void assertRecipients(String email) {
		Assert.assertTrue("No To", email.contains(EMAIL2));
		Assert.assertTrue("No To", email.contains(EMAIL3));
		Assert.assertTrue("No CC", email.contains(EMAIL4));
		Assert.assertTrue("No CC", email.contains(EMAIL5));
		Assert.assertTrue("No BCC", email.contains(EMAIL6));
		Assert.assertTrue("No BCC", email.contains(EMAIL7));
	}
}
