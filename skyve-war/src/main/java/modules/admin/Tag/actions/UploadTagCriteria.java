package modules.admin.Tag.actions;

import java.io.InputStream;
import java.util.List;

import org.skyve.EXT;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.messages.UploadException.Problem;
import org.skyve.impl.bizport.AbstractDataFileLoader.LoaderActivityType;
import org.skyve.impl.bizport.DataFileField;
import org.skyve.impl.bizport.DataFileField.LoadAction;
import org.skyve.impl.bizport.POISheetLoader;
import org.skyve.metadata.controller.Upload;
import org.skyve.metadata.controller.UploadAction;
import org.skyve.tag.TagManager;
import org.skyve.web.WebContext;

import modules.admin.Tag.TagExtension;
import modules.admin.domain.Tag.FilterAction;

/**
 * Upload action that processes Excel files to tag or untag records based on matching criteria.
 * Supports equals, like, and contains operations on specified attributes, allowing bulk
 * tagging operations by uploading spreadsheet data with filter conditions.
 */
public class UploadTagCriteria extends UploadAction<TagExtension> {
	/**
	 * Processes an uploaded spreadsheet and tags or untags matching records.
	 *
	 * @param tag The tag definition receiving upload results.
	 * @param upload Uploaded spreadsheet metadata and stream.
	 * @param exception Collector for upload validation problems.
	 * @param webContext The current web context.
	 * @return The updated tag bean.
	 * @throws Exception If spreadsheet parsing or tagging fails.
	 */
	@Override
	public TagExtension upload(TagExtension tag,
			Upload upload,
			UploadException exception,
			WebContext webContext)
			throws Exception {
		if (!upload.getFileName().endsWith(MimeType.xlsx.getStandardFileSuffix())) {
			exception.addError(new Problem("Only " + MimeType.xlsx.name() + " files are supported", null));
			throw exception;
		}

		try (InputStream is = upload.getInputStream()) {
			SheetLoader loader = newSheetLoader(is, tag, exception);
			loader.setActivityType(LoaderActivityType.FIND);
			if (Boolean.TRUE.equals(tag.getFileHasHeaders())) {
				// skip headers
				loader.setDataIndex(1);
			}
			loader.setDebugMode(true);

			DataFileField searchField = new DataFileField(tag.getAttributeName(), 0);
			switch (tag.getFilterOperator()) {
				case equals:
					searchField.setLoadAction(LoadAction.LOOKUP_EQUALS);
					break;
				case like:
					searchField.setLoadAction(LoadAction.LOOKUP_LIKE);
					break;
				case contains:
					searchField.setLoadAction(LoadAction.LOOKUP_CONTAINS);
					break;
				default:
					break;
			}
			// set column index
			if (tag.getFilterColumn() != null) {
				searchField.setIndex(tag.getFilterColumn().intValue() - 1);
			}
			loader.addField(searchField);

			List<Bean> beansToTag = loader.beanResults();
			TagManager tagManager = tagManager();
			for (Bean bean : beansToTag) {

				if (FilterAction.tagRecordsThatMatch.equals(tag.getFilterAction())) {
					tagManager.tag(tag.getBizId(), bean);
				} else if (FilterAction.unTagRecordsThatMatch.equals(tag.getFilterAction())) {
					// remove the tagged record
					tagManager.untag(tag.getBizId(), bean);
				}
			}
			tag.setUploaded(Long.valueOf(loader.getDataIndex()));
			tag.setUploadMatched(Long.valueOf(beansToTag.size()));

			// reset counts
			tag.setTotalTagged(Long.valueOf(tag.count()));
			tag.setUploadTagged(Long.valueOf(tag.countDocument(tag.getUploadModuleName(), tag.getUploadDocumentName())));
		}

		return tag;
	}

	@SuppressWarnings("static-method") // test seam
	protected SheetLoader newSheetLoader(InputStream is, TagExtension tag, UploadException exception) throws Exception {
		return new POISheetLoaderAdapter(new POISheetLoader(is, 0, tag.getUploadModuleName(), tag.getUploadDocumentName(), exception));
	}

	@SuppressWarnings("static-method") // test seam
	protected TagManager tagManager() {
		return EXT.getTagManager();
	}

	protected interface SheetLoader {
		void setActivityType(LoaderActivityType activityType);

		void setDataIndex(int dataIndex);

		void setDebugMode(boolean debugMode);

		void addField(DataFileField field);

		List<Bean> beanResults();

		int getDataIndex();
	}

	private static class POISheetLoaderAdapter implements SheetLoader {
		private final POISheetLoader loader;

		private POISheetLoaderAdapter(POISheetLoader loader) {
			this.loader = loader;
		}

		@Override
		public void setActivityType(LoaderActivityType activityType) {
			loader.setActivityType(activityType);
		}

		@Override
		public void setDataIndex(int dataIndex) {
			loader.setDataIndex(dataIndex);
		}

		@Override
		public void setDebugMode(boolean debugMode) {
			loader.setDebugMode(debugMode);
		}

		@Override
		public void addField(DataFileField field) {
			loader.addField(field);
		}

		@Override
		public List<Bean> beanResults() {
			return loader.beanResults();
		}

		@Override
		public int getDataIndex() {
			return loader.getDataIndex();
		}
	}
}
