package modules.admin.Tag;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.List;

import org.junit.jupiter.api.Test;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class DeleteAllTaggedDataForTagJobTest extends AbstractH2Test {
	@Test
	void cancelReturnsNull() {
		assertNull(new DeleteAllTaggedDataForTagJob().cancel());
	}

	@Test
	void executeWithNoTaggedItemsLogsLifecycleAndCompletes() throws Exception {
		TagService tagService = mock(TagService.class);
		when(tagService.getTaggedItemsForDocument(any(), anyString(), anyString())).thenReturn(List.of());
		TagExtension tag = new TagExtension();
		tag.setBizId("tag-" + System.nanoTime());
		DeleteAllTaggedDataForTagJob job = new DeleteAllTaggedDataForTagJob();
		job.setBean(tag);
		injectTagService(job, tagService);

		job.execute();

		assertEquals(100, job.getPercentComplete());
		assertThat(job.getLog().get(0), containsString("Started Delete All Tagged Data Job"));
		assertThat(job.getLog().get(job.getLog().size() - 1), containsString("Finished Delete All Tagged Data Job"));
	}

	private static void injectTagService(DeleteAllTaggedDataForTagJob job, TagService tagService) throws Exception {
		Field field = DeleteAllTaggedDataForTagJob.class.getDeclaredField("tagService");
		field.setAccessible(true);
		field.set(job, tagService);
	}
}
