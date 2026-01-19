package modules.admin.Communication.actions;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.tag.TagManager;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import jakarta.inject.Inject;
import modules.admin.Communication.CommunicationExtension;
import modules.admin.Contact.ContactExtension;
import modules.admin.Tag.TagExtension;
import modules.admin.domain.Communication;
import modules.admin.domain.Contact;
import modules.admin.domain.Tag;
import util.AbstractH2Test;

/**
 * Tests for the GetCount action.
 */
public class GetCountH2Test extends AbstractH2Test {

	private DataBuilder db;
	private CommunicationExtension communication;
	private TagExtension tag;
	
	@Inject
	private GetCount action;

	@BeforeEach
	public void setup() {
		db = new DataBuilder().fixture(FixtureType.crud);

		communication = db.build(Communication.MODULE_NAME, Communication.DOCUMENT_NAME);
		tag = db.build(Tag.MODULE_NAME, Tag.DOCUMENT_NAME);
	}

	@Test
	public void testExecuteSetsResultsWithCount() throws Exception {
		// setup the test data
		setupValidCommunication();
		
		// Create and tag some contacts
		ContactExtension contact1 = db.build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);
		contact1 = CORE.getPersistence().save(contact1);
		ContactExtension contact2 = db.build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);
		contact2 = CORE.getPersistence().save(contact2);
		
		TagManager tm = EXT.getTagManager();
		tm.tag(tag.getBizId(), contact1);
		tm.tag(tag.getBizId(), contact2);
		
		// call the method under test
		ServerSideActionResult<Communication> result = action.execute(communication, null);
		
		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(communication));
		assertThat(communication.getResults(), is(notNullValue()));
		assertThat(communication.getResults(), containsString("2 communications"));
		assertThat(communication.getResults(), containsString("Contact"));
	}

	@Test
	public void testExecuteWithNoTaggedItems() throws Exception {
		// setup the test data with no tagged items
		setupValidCommunication();
		
		// call the method under test
		ServerSideActionResult<Communication> result = action.execute(communication, null);
		
		// verify the result shows 0 communications
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(communication));
		assertThat(communication.getResults(), is(notNullValue()));
		assertThat(communication.getResults(), containsString("0 communications"));
		assertThat(communication.getResults(), containsString("Contact"));
	}

	/**
	 * Helper method to set up a valid communication with required fields.
	 */
	private void setupValidCommunication() {
		tag = CORE.getPersistence().save(tag);
		communication.setTag(tag);
		communication.setModuleName("admin");
		communication.setDocumentName("Contact");
	}
}
