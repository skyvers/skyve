package modules.admin.Tag.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.lang.reflect.Constructor;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.messages.UploadException.Problem;
import org.skyve.impl.bizport.AbstractDataFileLoader.LoaderActivityType;
import org.skyve.impl.bizport.DataFileField;
import org.skyve.impl.bizport.DataFileField.LoadAction;
import org.skyve.impl.bizport.POISheetLoader;
import org.skyve.metadata.controller.Upload;
import org.skyve.metadata.controller.WebFileInputStream;
import org.skyve.tag.TagManager;

import modules.admin.Tag.TagExtension;
import modules.admin.domain.Tag.FilterAction;

@SuppressWarnings("static-method")
class UploadTagCriteriaTest {
	@Test
	void uploadWithUnsupportedExtensionAddsErrorAndThrowsUploadException() {
		UploadTagCriteria action = new UploadTagCriteria();
		TagExtension bean = new TagExtension();
		Upload upload = upload("criteria.csv");
		UploadException uploadException = new UploadException();
		UploadException thrown = assertThrows(UploadException.class,
				() -> action.upload(bean, upload, uploadException, null));

		Problem problem = thrown.getErrors().iterator().next();
		assertThat(problem.getWhat(), is("Only xlsx files are supported"));
	}

	@Test
	void uploadTagsMatchingRowsUsingEqualsAndHeaderOffset() throws Exception {
		Bean result = mock(Bean.class);
		FakeSheetLoader loader = new FakeSheetLoader(List.of(result), 5);
		TagManager tagManager = mock(TagManager.class);
		TestableUploadTagCriteria action = new TestableUploadTagCriteria(loader, tagManager);
		TestTag tag = tag(FilterAction.tagRecordsThatMatch);
		tag.setFilterOperator(modules.admin.domain.Tag.FilterOperator.equals);
		tag.setFilterColumn(Integer.valueOf(3));
		tag.setFileHasHeaders(Boolean.TRUE);

		TagExtension returned = action.upload(tag, upload("criteria.xlsx"), new UploadException(), null);

		assertThat(returned, is(tag));
		assertEquals(LoaderActivityType.FIND, loader.activityType);
		assertEquals(1, loader.dataIndex);
		assertEquals(Boolean.TRUE, Boolean.valueOf(loader.debugMode));
		assertEquals("email1", loader.field.getBinding());
		assertEquals(Integer.valueOf(2), loader.field.getIndex());
		assertEquals(LoadAction.LOOKUP_EQUALS, loader.field.getLoadAction());
		verify(tagManager).tag("tag-id", result);
		assertEquals(Long.valueOf(5), tag.getUploaded());
		assertEquals(Long.valueOf(1), tag.getUploadMatched());
		assertEquals(Long.valueOf(17), tag.getTotalTagged());
		assertEquals(Long.valueOf(9), tag.getUploadTagged());
	}

	@Test
	void uploadUntagsMatchingRowsUsingLikeWithoutHeaders() throws Exception {
		Bean result = mock(Bean.class);
		FakeSheetLoader loader = new FakeSheetLoader(List.of(result), 2);
		TagManager tagManager = mock(TagManager.class);
		TestableUploadTagCriteria action = new TestableUploadTagCriteria(loader, tagManager);
		TestTag tag = tag(FilterAction.unTagRecordsThatMatch);
		tag.setFilterOperator(modules.admin.domain.Tag.FilterOperator.like);
		tag.setFileHasHeaders(Boolean.FALSE);

		action.upload(tag, upload("criteria.xlsx"), new UploadException(), null);

		assertEquals(0, loader.dataIndex);
		assertEquals(Integer.valueOf(0), loader.field.getIndex());
		assertEquals(LoadAction.LOOKUP_LIKE, loader.field.getLoadAction());
		verify(tagManager).untag("tag-id", result);
	}

	@Test
	void uploadUsesContainsLookupAndDoesNotTagWhenActionIsUnset() throws Exception {
		FakeSheetLoader loader = new FakeSheetLoader(List.of(mock(Bean.class)), 4);
		TagManager tagManager = mock(TagManager.class);
		TestableUploadTagCriteria action = new TestableUploadTagCriteria(loader, tagManager);
		TestTag tag = tag(null);
		tag.setFilterOperator(modules.admin.domain.Tag.FilterOperator.contains);

		action.upload(tag, upload("criteria.xlsx"), new UploadException(), null);

		assertEquals(LoadAction.LOOKUP_CONTAINS, loader.field.getLoadAction());
		verifyNoInteractions(tagManager);
		assertEquals(Long.valueOf(1), tag.getUploadMatched());
	}

	@Test
	@SuppressWarnings("boxing")
	void poiSheetLoaderAdapterDelegatesToWrappedLoader() throws Exception {
		POISheetLoader wrapped = mock(POISheetLoader.class);
		DataFileField field = new DataFileField("email1", 0);
		List<Bean> results = List.of(mock(Bean.class));
		when(wrapped.beanResults()).thenReturn(results);
		when(wrapped.getDataIndex()).thenReturn(Integer.valueOf(3));
		UploadTagCriteria.SheetLoader adapter = privateAdapter(wrapped);

		adapter.setActivityType(LoaderActivityType.FIND);
		adapter.setDataIndex(1);
		adapter.setDebugMode(true);
		adapter.addField(field);

		assertEquals(results, adapter.beanResults());
		assertEquals(Integer.valueOf(3), Integer.valueOf(adapter.getDataIndex()));
		verify(wrapped).setActivityType(LoaderActivityType.FIND);
		verify(wrapped).setDataIndex(1);
		verify(wrapped).setDebugMode(true);
		verify(wrapped).addField(field);
	}

	@SuppressWarnings("resource")
	private static Upload upload(String fileName) {
		return new Upload(fileName,
				new WebFileInputStream(new ByteArrayInputStream("code".getBytes(StandardCharsets.UTF_8))),
				MimeType.csv);
	}

	private static TestTag tag(FilterAction filterAction) {
		TestTag tag = new TestTag();
		tag.setBizId("tag-id");
		tag.setUploadModuleName("admin");
		tag.setUploadDocumentName("User");
		tag.setAttributeName("email1");
		tag.setFilterAction(filterAction);
		return tag;
	}

	private static UploadTagCriteria.SheetLoader privateAdapter(POISheetLoader wrapped) throws Exception {
		Class<?> adapterClass = Class.forName(UploadTagCriteria.class.getName() + "$POISheetLoaderAdapter");
		Constructor<?> constructor = adapterClass.getDeclaredConstructor(POISheetLoader.class);
		constructor.setAccessible(true);
		return UploadTagCriteria.SheetLoader.class.cast(constructor.newInstance(wrapped));
	}

	private static class TestTag extends TagExtension {
		private static final long serialVersionUID = -4365883137790866813L;

		@Override
		public long count() {
			return 17;
		}

		@Override
		public long countDocument(String moduleName, String documentName) {
			return 9;
		}
	}

	private static class TestableUploadTagCriteria extends UploadTagCriteria {
		private final FakeSheetLoader loader;
		private final TagManager tagManager;

		private TestableUploadTagCriteria(FakeSheetLoader loader, TagManager tagManager) {
			this.loader = loader;
			this.tagManager = tagManager;
		}

		@Override
		protected SheetLoader newSheetLoader(InputStream is, TagExtension tag, UploadException exception) {
			return loader;
		}

		@Override
		protected TagManager tagManager() {
			return tagManager;
		}
	}

	private static class FakeSheetLoader implements UploadTagCriteria.SheetLoader {
		private final List<Bean> results;
		private final int finalDataIndex;
		private LoaderActivityType activityType;
		private int dataIndex;
		private boolean debugMode;
		private DataFileField field;

		private FakeSheetLoader(List<Bean> results, int finalDataIndex) {
			this.results = results;
			this.finalDataIndex = finalDataIndex;
		}

		@Override
		public void setActivityType(LoaderActivityType activityType) {
			this.activityType = activityType;
		}

		@Override
		public void setDataIndex(int dataIndex) {
			this.dataIndex = dataIndex;
		}

		@Override
		public void setDebugMode(boolean debugMode) {
			this.debugMode = debugMode;
		}

		@Override
		public void addField(DataFileField f) {
			this.field = f;
		}

		@Override
		public List<Bean> beanResults() {
			return results;
		}

		@Override
		public int getDataIndex() {
			return finalDataIndex;
		}
	}
}
