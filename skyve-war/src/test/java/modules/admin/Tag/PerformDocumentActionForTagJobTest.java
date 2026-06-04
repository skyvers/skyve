package modules.admin.Tag;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.lang.reflect.Field;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;

import modules.admin.domain.User;
import util.AbstractH2Test;

/**
 * Tests simple job branches for tagged document actions.
 */
@SuppressWarnings("static-method")
class PerformDocumentActionForTagJobTest extends AbstractH2Test {
	@Test
	void cancelReturnsNull() {
		assertNull(new PerformDocumentActionForTagJob().cancel());
	}

	@Test
	void executeWithNoDocumentActionCompletesWithoutProcessingItems() throws Exception {
		PerformDocumentActionForTagJob job = new PerformDocumentActionForTagJob();
		job.setBean(new TagExtension());

		job.execute();

		assertEquals(100, job.getPercentComplete());
		assertEquals(2, job.getLog().size());
		assertThat(job.getLog().get(0), containsString("Started Document Action"));
		assertThat(job.getLog().get(1), containsString("Finished Document Action"));
	}

	@Test
	void executeWithDefaultActionAndNoTaggedItemsLogsLifecycleAndCompletes() throws Exception {
		TagService tagService = mock(TagService.class);
		when(tagService.getTaggedItemsForDocument(any(), eq(User.MODULE_NAME), eq(User.DOCUMENT_NAME)))
				.thenReturn(List.<Bean>of());

		TagExtension tag = new TagExtension();
		tag.setDocumentAction(TagDefaultAction.tagValidate.toCode());
		tag.setActionModuleName(User.MODULE_NAME);
		tag.setActionDocumentName(User.DOCUMENT_NAME);
		tag.setNotification(Boolean.FALSE);
		PerformDocumentActionForTagJob job = new PerformDocumentActionForTagJob();
		job.setBean(tag);
		injectTagService(job, tagService);

		job.execute();

		assertEquals(100, job.getPercentComplete());
		assertEquals(2, job.getLog().size());
		assertThat(job.getLog().get(0), containsString("Started Document Action"));
		assertThat(job.getLog().get(1), containsString("Finished Document Action"));
	}

	private static void injectTagService(PerformDocumentActionForTagJob job, TagService tagService) throws Exception {
		Field field = PerformDocumentActionForTagJob.class.getDeclaredField("tagService");
		field.setAccessible(true);
		field.set(job, tagService);
	}
}
