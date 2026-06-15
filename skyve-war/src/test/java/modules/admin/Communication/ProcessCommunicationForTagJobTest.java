package modules.admin.Communication;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.nullable;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.lang.reflect.Field;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.app.admin.Communication.ActionType;

import modules.admin.Tag.TagService;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class ProcessCommunicationForTagJobTest extends AbstractH2Test {
	@Test
	void cancelReturnsNull() {
		assertNull(new ProcessCommunicationForTagJob().cancel());
	}

	@Test
	void executeWithNoActionTypeThrowsValidationFailure() {
		ProcessCommunicationForTagJob job = new ProcessCommunicationForTagJob();
		job.setBean(new CommunicationExtension());

		Exception exception = assertThrows(Exception.class, job::execute);

		assertThat(exception.getMessage(), containsString("no valid action type was selected"));
	}

	@Test
	void executeWithNoTaggedItemsLogsLifecycleAndCompletes() throws Exception {
		TagService tagService = mock(TagService.class);
		when(tagService.getTaggedItemsForDocument(any(), nullable(String.class), nullable(String.class)))
				.thenReturn(List.<Bean>of());

		CommunicationExtension communication = new CommunicationExtension();
		communication.setActionType(ActionType.testBindingsAndOutput);
		communication.setDescription("No recipients");
		communication.setDocumentName("User");
		communication.setNotification(Boolean.FALSE);
		ProcessCommunicationForTagJob job = new ProcessCommunicationForTagJob();
		job.setBean(communication);
		injectTagService(job, tagService);

		job.execute();

		assertEquals(100, job.getPercentComplete());
		assertEquals(2, job.getLog().size());
		assertThat(job.getLog().get(0), containsString("Started Processing Communication"));
		assertThat(job.getLog().get(0), containsString("0 tagged User documents"));
		assertThat(job.getLog().get(1), containsString("Finished Processing Communication"));
	}

	private static void injectTagService(ProcessCommunicationForTagJob job, TagService tagService) throws Exception {
		Field field = ProcessCommunicationForTagJob.class.getDeclaredField("tagService");
		field.setAccessible(true);
		field.set(job, tagService);
	}
}
