package modules.admin.Tag.actions;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.web.WebContext;

import modules.admin.Tag.TagDefaultAction;
import modules.admin.Tag.TagExtension;
import modules.admin.domain.Tag;

@SuppressWarnings("static-method")
class BulkDocumentActionTest {
	@Test
	void executeStartsJobAndSummarisesDefaultAction() throws Exception {
		TestableBulkDocumentAction action = new TestableBulkDocumentAction();
		TagExtension tag = tag(TagDefaultAction.tagValidate.toCode(), null);

		ServerSideActionResult<TagExtension> result = action.execute(tag, mock(WebContext.class));

		assertSame(tag, result.getBean());
		assertSame(tag, action.scheduledTag);
		assertEquals(1, action.runCount);
		assertEquals(1, action.growlCount);
		assertThat(tag.getDocumentActionResults(), containsString("Perform action: Validate"));
		assertThat(tag.getDocumentActionResults(), containsString("For all tagged instances of: Admin Users"));
		assertThat(tag.getDocumentActionResults(), containsString("Tag action Job started successfully"));
	}

	@Test
	void executeSummarisesCustomActionAndCondition() throws Exception {
		TestableBulkDocumentAction action = new TestableBulkDocumentAction();
		TagExtension tag = tag("customAction", "bean.enabled");

		action.execute(tag, mock(WebContext.class));

		assertThat(tag.getDocumentActionResults(), containsString("Perform action: customAction"));
		assertThat(tag.getDocumentActionResults(), containsString("Meeting condition: bean.enabled"));
	}

	private static TagExtension tag(String documentAction, String condition) {
		TagExtension tag = new TagExtension();
		tag.setDocumentAction(documentAction);
		tag.setActionModuleName("admin");
		tag.setActionDocumentName("User");
		tag.setDocumentCondition(condition);
		return tag;
	}

	private static class TestableBulkDocumentAction extends BulkDocumentAction {
		private final User user = mock(User.class);
		private final Customer customer = mock(Customer.class);
		private final JobMetaData job = mock(JobMetaData.class);
		private final Module module = mock(Module.class);
		private final Document document = mock(Document.class);
		private Tag scheduledTag;
		private int runCount;
		private int growlCount;

		private TestableBulkDocumentAction() {
			when(user.getCustomer()).thenReturn(customer);
			when(module.getLocalisedTitle()).thenReturn("Admin");
			when(document.getLocalisedPluralAlias()).thenReturn("Users");
		}

		@Override
		protected User getUser() {
			return user;
		}

		@Override
		protected JobMetaData getJob(Customer c) {
			return job;
		}

		@Override
		protected void runJob(JobMetaData jobToRun, TagExtension tag, User userToRun) {
			runCount++;
			scheduledTag = tag;
		}

		@Override
		protected Module getModule(Customer c, String moduleName) {
			return module;
		}

		@Override
		protected Document getDocument(Module m, Customer c, String documentName) {
			return document;
		}

		@Override
		protected void growl(WebContext webContext) {
			growlCount++;
		}
	}
}
