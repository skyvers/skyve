package modules.admin.Tag.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.tag.TagManager;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.Tag.TagExtension;
import modules.admin.User.UserExtension;
import modules.admin.UserProxy.UserProxyExtension;
import modules.admin.domain.Contact;
import modules.admin.domain.Tag;
import modules.admin.domain.Tagged;
import modules.admin.domain.User;
import util.AbstractH2Test;

class CopyTagToUserH2Test extends AbstractH2Test {

	private DataBuilder db;
	private CopyTagToUser action;
	private TagManager tagManager;

	@BeforeEach
	void setup() {
		db = new DataBuilder().fixture(FixtureType.crud);
		action = new CopyTagToUser();
		tagManager = EXT.getTagManager();
	}

	@Test
	void testExecuteCopiesTagToUser() throws Exception {
		long suffix = System.nanoTime();
		String tagName = "Test Tag " + suffix;
		// Create source and target users
		UserExtension sourceUser = db.build(User.MODULE_NAME, User.DOCUMENT_NAME);
		sourceUser.getContact().setEmail1("source-" + suffix + "@test.com");
		sourceUser = CORE.getPersistence().save(sourceUser);

		UserExtension targetUser = db.build(User.MODULE_NAME, User.DOCUMENT_NAME);
		targetUser.getContact().setEmail1("target-" + suffix + "@test.com");
		targetUser = CORE.getPersistence().save(targetUser);

		// Create tag for source user
		TagExtension sourceTag = Tag.newInstance();
		sourceTag.setName(tagName);
		sourceTag.setBizUserId(sourceUser.getBizId());
		sourceTag = CORE.getPersistence().save(sourceTag);

		// Create some contacts to tag
		Contact contact1 = db.build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);
		contact1 = CORE.getPersistence().save(contact1);
		Contact contact2 = db.build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);
		contact2 = CORE.getPersistence().save(contact2);

		// Tag the contacts
		tagManager.tag(sourceTag.getBizId(), contact1);
		tagManager.tag(sourceTag.getBizId(), contact2);

		// Verify source tag has 2 tagged items
		assertEquals(2L, sourceTag.count());

		// Set up the action bean with copyToUser
		UserProxyExtension targetUserProxy = targetUser.toUserProxy();
		sourceTag.setCopyToUser(targetUserProxy);

		// Execute the action
		ServerSideActionResult<TagExtension> result = action.execute(sourceTag, new MockWebContext());

		// Verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(notNullValue()));

		// Verify a new tag was created for the target user
		DocumentQuery tagQuery = CORE.getPersistence().newDocumentQuery(Tag.MODULE_NAME, Tag.DOCUMENT_NAME);
		tagQuery.getFilter().addEquals(Tag.namePropertyName, tagName);
		tagQuery.getFilter().addEquals(Bean.USER_ID, targetUser.getBizId());
		TagExtension newTag = tagQuery.beanResult();
		assertNotNull(newTag);

		assertThat(newTag, is(notNullValue()));
		assertThat(newTag.getName(), is(tagName));
		assertThat(newTag.getBizUserId(), is(targetUser.getBizId()));

		// Verify the new tag has the same 2 tagged items
		assertEquals(2L, newTag.count());

		// Verify the original tag still has 2 tagged items (unchanged)
		TagExtension originalTag = CORE.getPersistence().retrieve(Tag.MODULE_NAME, Tag.DOCUMENT_NAME, sourceTag.getBizId());
		assertNotNull(originalTag);
		assertEquals(2L, originalTag.count());

		// Verify the tagged items are the same for both tags
		DocumentQuery taggedQuery1 = CORE.getPersistence().newDocumentQuery(Tagged.MODULE_NAME, Tagged.DOCUMENT_NAME);
		taggedQuery1.getFilter().addEquals(Tagged.tagPropertyName, sourceTag);
		taggedQuery1.addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "CountOfId");
		Number originalTagged = taggedQuery1.scalarResult(Number.class);
		assertNotNull(originalTagged);
		long originalTaggedCount = originalTagged.longValue();

		DocumentQuery taggedQuery2 = CORE.getPersistence().newDocumentQuery(Tagged.MODULE_NAME, Tagged.DOCUMENT_NAME);
		taggedQuery2.getFilter().addEquals(Tagged.tagPropertyName, newTag);
		taggedQuery2.addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "CountOfId");
		Number newTagged = taggedQuery2.scalarResult(Number.class);
		assertNotNull(newTagged);
		long newTaggedCount = newTagged.longValue();

		assertEquals(2L, originalTaggedCount);
		assertEquals(2L, newTaggedCount);
	}

	@Test
	void testExecuteWithNullCopyToUserThrowsValidationException() throws Exception {
		long suffix = System.nanoTime();
		// Create a user and tag
		UserExtension user = db.build(User.MODULE_NAME, User.DOCUMENT_NAME);
		user.getContact().setEmail1("user-" + suffix + "@test.com");
		user = CORE.getPersistence().save(user);

		TagExtension tag = Tag.newInstance();
		tag.setName("Test Tag " + suffix);
		tag.setBizUserId(user.getBizId());
		tag = CORE.getPersistence().save(tag);

		// Tag a contact
		Contact contact = db.build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);
		contact = CORE.getPersistence().save(contact);
		tagManager.tag(tag.getBizId(), contact);

		// Set up the action bean without copyToUser
		tag.setCopyToUser(null);
		TagExtension actionBean = tag;

		// Execute the action and verify it throws ValidationException
		MockWebContext ctx = new MockWebContext();
		ValidationException exception = assertThrows(ValidationException.class, () -> action.execute(actionBean, ctx));

		// Verify the exception message
		assertEquals(1, exception.getMessages().size());
		assertThat(exception.getMessages().get(0).getText(), is("You have not selected a user to copy to."));

		// Verify no new tag was created
		DocumentQuery tagQuery = CORE.getPersistence().newDocumentQuery(Tag.MODULE_NAME, Tag.DOCUMENT_NAME);
		tagQuery.getFilter().addEquals(Bean.USER_ID, user.getBizId());
		tagQuery.addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "CountOfId");
		Number tagCountNumber = tagQuery.scalarResult(Number.class);
		assertNotNull(tagCountNumber);
		long tagCount = tagCountNumber.longValue();
		assertEquals(1L, tagCount); // Only the original tag

		// Verify the original tag still has 1 tagged item
		TagExtension originalTag = CORE.getPersistence().retrieve(Tag.MODULE_NAME, Tag.DOCUMENT_NAME, tag.getBizId());
		assertNotNull(originalTag);
		assertEquals(1L, originalTag.count());
	}

	@Test
	void testExecuteWithEmptyTagCopiesEmptyTag() throws Exception {
		long suffix = System.nanoTime();
		String tagName = "Empty Tag " + suffix;
		// Create source and target users
		UserExtension sourceUser = db.build(User.MODULE_NAME, User.DOCUMENT_NAME);
		sourceUser.getContact().setEmail1("source-empty-" + suffix + "@test.com");
		sourceUser = CORE.getPersistence().save(sourceUser);

		UserExtension targetUser = db.build(User.MODULE_NAME, User.DOCUMENT_NAME);
		targetUser.getContact().setEmail1("target-empty-" + suffix + "@test.com");
		targetUser = CORE.getPersistence().save(targetUser);

		// Create empty tag for source user (no tagged items)
		TagExtension sourceTag = Tag.newInstance();
		sourceTag.setName(tagName);
		sourceTag.setBizUserId(sourceUser.getBizId());
		sourceTag = CORE.getPersistence().save(sourceTag);

		// Verify source tag has 0 tagged items
		assertEquals(0L, sourceTag.count());

		// Set up the action bean with copyToUser
		UserProxyExtension targetUserProxy = targetUser.toUserProxy();
		sourceTag.setCopyToUser(targetUserProxy);

		// Execute the action
		ServerSideActionResult<TagExtension> result = action.execute(sourceTag, new MockWebContext());

		// Verify the result
		assertThat(result, is(notNullValue()));

		// Verify a new tag was created for the target user
		DocumentQuery tagQuery = CORE.getPersistence().newDocumentQuery(Tag.MODULE_NAME, Tag.DOCUMENT_NAME);
		tagQuery.getFilter().addEquals(Tag.namePropertyName, tagName);
		tagQuery.getFilter().addEquals(Bean.USER_ID, targetUser.getBizId());
		TagExtension newTag = tagQuery.beanResult();
		assertNotNull(newTag);

		assertThat(newTag, is(notNullValue()));
		assertThat(newTag.getName(), is(tagName));
		assertThat(newTag.getBizUserId(), is(targetUser.getBizId()));

		// Verify the new tag has 0 tagged items
		assertEquals(0L, newTag.count());
	}
}
