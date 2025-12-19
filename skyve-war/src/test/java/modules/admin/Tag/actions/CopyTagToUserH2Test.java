package modules.admin.Tag.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

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

public class CopyTagToUserH2Test extends AbstractH2Test {

	private DataBuilder db;
	private CopyTagToUser action;
	private TagManager tagManager;

	@BeforeEach
	public void setup() throws Exception {
		db = new DataBuilder().fixture(FixtureType.crud);
		action = new CopyTagToUser();
		tagManager = EXT.getTagManager();
	}

	@Test
	@SuppressWarnings("boxing")
	public void testExecuteCopiesTagToUser() throws Exception {
		// Create source and target users
		UserExtension sourceUser = db.build(User.MODULE_NAME, User.DOCUMENT_NAME);
		sourceUser.getContact().setEmail1("source@test.com");
		sourceUser = CORE.getPersistence().save(sourceUser);

		UserExtension targetUser = db.build(User.MODULE_NAME, User.DOCUMENT_NAME);
		targetUser.getContact().setEmail1("target@test.com");
		targetUser = CORE.getPersistence().save(targetUser);

		// Create tag for source user
		TagExtension sourceTag = Tag.newInstance();
		sourceTag.setName("Test Tag");
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
		CORE.getPersistence().commit(false);
		CORE.getPersistence().begin();

		// Verify source tag has 2 tagged items
		assertThat(sourceTag.count(), is(2L));

		// Set up the action bean with copyToUser
		TagExtension actionBean = CORE.getPersistence().retrieve(Tag.MODULE_NAME, Tag.DOCUMENT_NAME, sourceTag.getBizId());
		UserProxyExtension targetUserProxy = targetUser.toUserProxy();
		actionBean.setCopyToUser(targetUserProxy);

		// Execute the action
		ServerSideActionResult<TagExtension> result = action.execute(actionBean, new MockWebContext());

		// Verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(notNullValue()));

		// Verify a new tag was created for the target user
		DocumentQuery tagQuery = CORE.getPersistence().newDocumentQuery(Tag.MODULE_NAME, Tag.DOCUMENT_NAME);
		tagQuery.getFilter().addEquals(Tag.namePropertyName, "Test Tag");
		tagQuery.getFilter().addEquals(Bean.USER_ID, targetUser.getBizId());
		TagExtension newTag = tagQuery.beanResult();

		assertThat(newTag, is(notNullValue()));
		assertThat(newTag.getName(), is("Test Tag"));
		assertThat(newTag.getBizUserId(), is(targetUser.getBizId()));

		// Verify the new tag has the same 2 tagged items
		assertThat(newTag.count(), is(2L));

		// Verify the original tag still has 2 tagged items (unchanged)
		TagExtension originalTag = CORE.getPersistence().retrieve(Tag.MODULE_NAME, Tag.DOCUMENT_NAME, sourceTag.getBizId());
		assertThat(originalTag.count(), is(2L));

		// Verify the tagged items are the same for both tags
		DocumentQuery taggedQuery1 = CORE.getPersistence().newDocumentQuery(Tagged.MODULE_NAME, Tagged.DOCUMENT_NAME);
		taggedQuery1.getFilter().addEquals(Tagged.tagPropertyName, sourceTag);
		taggedQuery1.addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "CountOfId");
		long originalTaggedCount = taggedQuery1.scalarResult(Number.class).longValue();

		DocumentQuery taggedQuery2 = CORE.getPersistence().newDocumentQuery(Tagged.MODULE_NAME, Tagged.DOCUMENT_NAME);
		taggedQuery2.getFilter().addEquals(Tagged.tagPropertyName, newTag);
		taggedQuery2.addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "CountOfId");
		long newTaggedCount = taggedQuery2.scalarResult(Number.class).longValue();

		assertThat(originalTaggedCount, is(2L));
		assertThat(newTaggedCount, is(2L));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testExecuteWithNullCopyToUserThrowsValidationException() throws Exception {
		// Create a user and tag
		UserExtension user = db.build(User.MODULE_NAME, User.DOCUMENT_NAME);
		user.getContact().setEmail1("user@test.com");
		user = CORE.getPersistence().save(user);

		TagExtension tag = Tag.newInstance();
		tag.setName("Test Tag");
		tag.setBizUserId(user.getBizId());
		tag = CORE.getPersistence().save(tag);

		// Tag a contact
		Contact contact = db.build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);
		contact = CORE.getPersistence().save(contact);
		tagManager.tag(tag.getBizId(), contact);
		CORE.getPersistence().commit(false);
		CORE.getPersistence().begin();

		// Set up the action bean without copyToUser
		TagExtension actionBean = CORE.getPersistence().retrieve(Tag.MODULE_NAME, Tag.DOCUMENT_NAME, tag.getBizId());
		actionBean.setCopyToUser(null);

		// Execute the action and verify it throws ValidationException
		ValidationException exception = assertThrows(ValidationException.class, () -> {
			action.execute(actionBean, new MockWebContext());
		});

		// Verify the exception message
		assertThat(exception.getMessages().size(), is(1));
		assertThat(exception.getMessages().get(0).getText(), is("You have not selected a user to copy to."));

		// Verify no new tag was created
		DocumentQuery tagQuery = CORE.getPersistence().newDocumentQuery(Tag.MODULE_NAME, Tag.DOCUMENT_NAME);
		tagQuery.getFilter().addEquals(Bean.USER_ID, user.getBizId());
		tagQuery.addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "CountOfId");
		long tagCount = tagQuery.scalarResult(Number.class).longValue();
		assertThat(tagCount, is(1L)); // Only the original tag

		// Verify the original tag still has 1 tagged item
		TagExtension originalTag = CORE.getPersistence().retrieve(Tag.MODULE_NAME, Tag.DOCUMENT_NAME, tag.getBizId());
		assertThat(originalTag.count(), is(1L));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testExecuteWithEmptyTagCopiesEmptyTag() throws Exception {
		// Create source and target users
		UserExtension sourceUser = db.build(User.MODULE_NAME, User.DOCUMENT_NAME);
		sourceUser.getContact().setEmail1("source@test.com");
		sourceUser = CORE.getPersistence().save(sourceUser);

		UserExtension targetUser = db.build(User.MODULE_NAME, User.DOCUMENT_NAME);
		targetUser.getContact().setEmail1("target@test.com");
		targetUser = CORE.getPersistence().save(targetUser);

		// Create empty tag for source user (no tagged items)
		TagExtension sourceTag = Tag.newInstance();
		sourceTag.setName("Empty Tag");
		sourceTag.setBizUserId(sourceUser.getBizId());
		sourceTag = CORE.getPersistence().save(sourceTag);

		// Verify source tag has 0 tagged items
		assertThat(sourceTag.count(), is(0L));

		// Set up the action bean with copyToUser
		TagExtension actionBean = CORE.getPersistence().retrieve(Tag.MODULE_NAME, Tag.DOCUMENT_NAME, sourceTag.getBizId());
		UserProxyExtension targetUserProxy = targetUser.toUserProxy();
		actionBean.setCopyToUser(targetUserProxy);

		// Execute the action
		ServerSideActionResult<TagExtension> result = action.execute(actionBean, new MockWebContext());

		// Verify the result
		assertThat(result, is(notNullValue()));

		// Verify a new tag was created for the target user
		DocumentQuery tagQuery = CORE.getPersistence().newDocumentQuery(Tag.MODULE_NAME, Tag.DOCUMENT_NAME);
		tagQuery.getFilter().addEquals(Tag.namePropertyName, "Empty Tag");
		tagQuery.getFilter().addEquals(Bean.USER_ID, targetUser.getBizId());
		TagExtension newTag = tagQuery.beanResult();

		assertThat(newTag, is(notNullValue()));
		assertThat(newTag.getName(), is("Empty Tag"));
		assertThat(newTag.getBizUserId(), is(targetUser.getBizId()));

		// Verify the new tag has 0 tagged items
		assertThat(newTag.count(), is(0L));
	}
}
