package modules.admin.ControlPanel;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.junit.jupiter.api.Test;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

import modules.admin.Tag.TagExtension;
import modules.admin.domain.Tag;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class GenerateTestDataJobTest extends AbstractH2Test {
	@Test
	void createOrRetrieveTagReturnsNullWhenTaggingDisabled() throws Exception {
		ControlPanelExtension bean = new ControlPanelExtension();
		bean.setTestTagGeneratedData(Boolean.FALSE);

		assertNull(invokeCreateOrRetrieveTag(mock(Persistence.class), bean));
	}

	@Test
	void createOrRetrieveTagReturnsExistingTagWhenFound() throws Exception {
		TagExtension existing = new TagExtension();
		existing.setName("Existing generated data");
		ControlPanelExtension bean = new ControlPanelExtension();
		bean.setTestTagGeneratedData(Boolean.TRUE);
		bean.setTestTagName(existing.getName());
		Persistence persistence = mock(Persistence.class);
		DocumentQuery query = mock(DocumentQuery.class);
		DocumentFilter filter = mock(DocumentFilter.class);
		when(persistence.newDocumentQuery(Tag.MODULE_NAME, Tag.DOCUMENT_NAME)).thenReturn(query);
		when(query.getFilter()).thenReturn(filter);
		when(query.beanResult()).thenReturn(existing);

		Tag tag = invokeCreateOrRetrieveTag(persistence, bean);

		assertThat(tag, is(existing));
	}

	@Test
	void createOrRetrieveTagCreatesVisibleTagWhenNotFound() throws Exception {
		ControlPanelExtension bean = new ControlPanelExtension();
		bean.setTestTagGeneratedData(Boolean.TRUE);
		bean.setTestTagName("Generated " + System.nanoTime());
		Persistence persistence = mock(Persistence.class);
		DocumentQuery query = mock(DocumentQuery.class);
		DocumentFilter filter = mock(DocumentFilter.class);
		when(persistence.newDocumentQuery(Tag.MODULE_NAME, Tag.DOCUMENT_NAME)).thenReturn(query);
		when(query.getFilter()).thenReturn(filter);
		when(query.beanResult()).thenReturn(null);
		when(persistence.save(any(Tag.class))).thenAnswer(invocation -> invocation.getArgument(0));

		Tag tag = invokeCreateOrRetrieveTag(persistence, bean);

		assertThat(tag.getName(), is(bean.getTestTagName()));
		assertThat(tag.getVisible(), is(Boolean.TRUE));
	}

	private static Tag invokeCreateOrRetrieveTag(Persistence persistence, ControlPanelExtension bean) throws Exception {
		Method method = GenerateTestDataJob.class.getDeclaredMethod("createOrRetrieveTag", Persistence.class,
				ControlPanelExtension.class);
		method.setAccessible(true);
		try {
			return (Tag) method.invoke(null, persistence, bean);
		} catch (InvocationTargetException e) {
			Throwable cause = e.getCause();
			if (cause instanceof Exception exception) {
				throw exception;
			}
			throw e;
		}
	}
}
