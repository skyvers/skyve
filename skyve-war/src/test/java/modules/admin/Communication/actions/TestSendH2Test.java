package modules.admin.Communication.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mockStatic;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.domain.app.admin.Communication.ActionType;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.tag.TagManager;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import org.skyve.web.WebContext;

import jakarta.inject.Inject;
import modules.admin.Contact.ContactExtension;
import modules.admin.Tag.TagService;
import modules.admin.User.UserService;
import modules.admin.UserProxy.UserProxyExtension;
import modules.admin.domain.Communication;
import modules.admin.domain.Contact;
import modules.admin.domain.Tag;
import modules.admin.domain.UserProxy;
import util.AbstractH2Test;

/**
 * Tests for the TestSend action.
 */
public class TestSendH2Test extends AbstractH2Test {

	private DataBuilder db;
	
	@Inject
	private TagService tagService;
	
	@Inject
	private UserService userService;
	
	private TestSend action;

	@BeforeEach
	public void setup() {
		db = new DataBuilder().fixture(FixtureType.crud);
		action = new TestSend();
	}

	@Test
	public void testExecuteThrowsValidationExceptionWhenNoTaggedItems() throws Exception {
		// setup the test data - create communication with a tag but no tagged items
		Communication communication = db.build(Communication.MODULE_NAME, Communication.DOCUMENT_NAME);
		Tag tag = CORE.getPersistence().save(db.build(Tag.MODULE_NAME, Tag.DOCUMENT_NAME));
		communication.setTag(tag);
		communication.setModuleName("admin");
		communication.setDocumentName("Contact");
		
		// Set up current user with contact
		setupCurrentUserWithContact();
		
		// call the method under test and expect exception since no items are tagged
		ValidationException exception = assertThrows(ValidationException.class, 
				() -> action.execute(communication, null));
		
		// verify the exception message
		assertThat(exception.getMessages().size(), is(1));
		assertThat(exception.getMessages().get(0).getText(), 
				is("There are no tagged items - tag at least 1 (one) item to test this communication."));
	}

	@Test
	public void testExecuteSuccessfullyWithTaggedItems() throws Exception {
		// setup the test data
		Communication communication = db.build(Communication.MODULE_NAME, Communication.DOCUMENT_NAME);
		Tag tag = CORE.getPersistence().save(db.build(Tag.MODULE_NAME, Tag.DOCUMENT_NAME));
		communication.setTag(tag);
		communication.setModuleName("admin");
		communication.setDocumentName("Contact");
		
		// Create and tag a contact
		ContactExtension taggedContact = db.build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);
		taggedContact = CORE.getPersistence().save(taggedContact);
		TagManager tm = EXT.getTagManager();
		tm.tag(tag.getBizId(), taggedContact);
		
		// Set up current user with contact
		setupCurrentUserWithContact();
		
		// call the method under test (mock CommunicationUtil to avoid actually sending)
		ServerSideActionResult<Communication> result;
		try (MockedStatic<org.skyve.util.CommunicationUtil> mockedUtil = 
				mockStatic(org.skyve.util.CommunicationUtil.class)) {
			// Mock the send method to do nothing
			mockedUtil.when(() -> org.skyve.util.CommunicationUtil.send(
					any(), any(), any(), any(), any(), any())).then(invocation -> null);
			
			result = action.execute(communication, null);
		}
		
		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(communication));
		assertThat(communication.getActionType(), is(ActionType.sendImmediately));
	}

	@Test
	public void testExecuteRestoresSendToOverrideOnSuccess() throws Exception {
		// setup the test data
		Communication communication = db.build(Communication.MODULE_NAME, Communication.DOCUMENT_NAME);
		String originalSendToOverride = "original@example.com";
		communication.setSendToOverride(originalSendToOverride);
		
		Tag tag = CORE.getPersistence().save(db.build(Tag.MODULE_NAME, Tag.DOCUMENT_NAME));
		communication.setTag(tag);
		communication.setModuleName("admin");
		communication.setDocumentName("Contact");
		
		// Create and tag a contact
		ContactExtension taggedContact = db.build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);
		taggedContact = CORE.getPersistence().save(taggedContact);
		TagManager tm = EXT.getTagManager();
		tm.tag(tag.getBizId(), taggedContact);
		
		// Set up current user with contact
		setupCurrentUserWithContact();
		
		// call the method under test
		try (MockedStatic<org.skyve.util.CommunicationUtil> mockedUtil = 
				mockStatic(org.skyve.util.CommunicationUtil.class)) {
			mockedUtil.when(() -> org.skyve.util.CommunicationUtil.send(
					any(), any(), any(), any(), any(), any())).then(invocation -> null);
			
			action.execute(communication, null);
		}
		
		// verify sendToOverride was restored to original value
		assertThat(communication.getSendToOverride(), is(originalSendToOverride));
	}

	@Test
	public void testExecuteRestoresSendToOverrideOnFailure() throws Exception {
		// setup the test data
		Communication communication = db.build(Communication.MODULE_NAME, Communication.DOCUMENT_NAME);
		String originalSendToOverride = "original@example.com";
		communication.setSendToOverride(originalSendToOverride);
		
		Tag tag = CORE.getPersistence().save(db.build(Tag.MODULE_NAME, Tag.DOCUMENT_NAME));
		communication.setTag(tag);
		communication.setModuleName("admin");
		communication.setDocumentName("Contact");
		
		// Create and tag a contact
		ContactExtension taggedContact = db.build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);
		taggedContact = CORE.getPersistence().save(taggedContact);
		TagManager tm = EXT.getTagManager();
		tm.tag(tag.getBizId(), taggedContact);
		
		// Set up current user with contact
		setupCurrentUserWithContact();
		
		// call the method under test with mocked CommunicationUtil throwing an exception
		try (MockedStatic<org.skyve.util.CommunicationUtil> mockedUtil = 
				mockStatic(org.skyve.util.CommunicationUtil.class)) {
			mockedUtil.when(() -> org.skyve.util.CommunicationUtil.send(
					any(), any(), any(), any(), any(), any()))
					.thenThrow(new RuntimeException("Simulated send failure"));
			
			assertThrows(RuntimeException.class, () -> action.execute(communication, null));
		}
		
		// verify sendToOverride was restored to original value even after exception
		assertThat(communication.getSendToOverride(), is(originalSendToOverride));
	}

	@Test
	public void testExecuteRestoresNullSendToOverride() throws Exception {
		// setup the test data
		Communication communication = db.build(Communication.MODULE_NAME, Communication.DOCUMENT_NAME);
		communication.setSendToOverride(null);
		
		Tag tag = CORE.getPersistence().save(db.build(Tag.MODULE_NAME, Tag.DOCUMENT_NAME));
		communication.setTag(tag);
		communication.setModuleName("admin");
		communication.setDocumentName("Contact");
		
		// Create and tag a contact
		ContactExtension taggedContact = db.build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);
		taggedContact = CORE.getPersistence().save(taggedContact);
		TagManager tm = EXT.getTagManager();
		tm.tag(tag.getBizId(), taggedContact);
		
		// Set up current user with contact
		setupCurrentUserWithContact();
		
		// call the method under test
		try (MockedStatic<org.skyve.util.CommunicationUtil> mockedUtil = 
				mockStatic(org.skyve.util.CommunicationUtil.class)) {
			mockedUtil.when(() -> org.skyve.util.CommunicationUtil.send(
					any(), any(), any(), any(), any(), any())).then(invocation -> null);
			
			action.execute(communication, null);
		}
		
		// verify sendToOverride was restored to null
		assertThat(communication.getSendToOverride(), is(nullValue()));
	}

	@Test
	public void testExecuteSetsActionTypeToSendImmediately() throws Exception {
		// setup the test data
		Communication communication = db.build(Communication.MODULE_NAME, Communication.DOCUMENT_NAME);
		communication.setActionType(null);
		
		Tag tag = CORE.getPersistence().save(db.build(Tag.MODULE_NAME, Tag.DOCUMENT_NAME));
		communication.setTag(tag);
		communication.setModuleName("admin");
		communication.setDocumentName("Contact");
		
		// Create and tag a contact
		ContactExtension taggedContact = db.build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);
		taggedContact = CORE.getPersistence().save(taggedContact);
		TagManager tm = EXT.getTagManager();
		tm.tag(tag.getBizId(), taggedContact);
		
		// Set up current user with contact
		setupCurrentUserWithContact();
		
		// call the method under test
		try (MockedStatic<org.skyve.util.CommunicationUtil> mockedUtil = 
				mockStatic(org.skyve.util.CommunicationUtil.class)) {
			mockedUtil.when(() -> org.skyve.util.CommunicationUtil.send(
					any(), any(), any(), any(), any(), any())).then(invocation -> null);
			
			action.execute(communication, null);
		}
		
		// verify actionType was set
		assertThat(communication.getActionType(), is(ActionType.sendImmediately));
	}

	/**
	 * Helper method to set up the current user with a contact that has an email.
	 * This is required for TestSend to work since it gets the email from the current user's contact.
	 */
	private void setupCurrentUserWithContact() {
		try {
			// Get or create UserProxy for current user
			UserProxyExtension userProxy = userService.currentAdminUserProxy();
			if (userProxy == null) {
				userProxy = db.build(UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME);
				userProxy.setBizId(CORE.getPersistence().getUser().getId());
			}
			
			// Ensure the user proxy has a contact with an email
			ContactExtension contact = userProxy.getContact();
			if (contact == null) {
				contact = db.build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);
			}
			if (contact.getEmail1() == null) {
				contact.setEmail1("testuser@example.com");
			}
			contact = CORE.getPersistence().save(contact);
			userProxy.setContact(contact);
			userProxy = CORE.getPersistence().save(userProxy);
		} catch (Exception e) {
			// If we can't set up the user, the test will fail appropriately
			throw new RuntimeException("Failed to set up current user with contact", e);
		}
	}
}
