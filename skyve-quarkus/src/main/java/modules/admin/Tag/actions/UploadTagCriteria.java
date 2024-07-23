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
import org.skyve.web.WebContext;

import modules.admin.Tag.TagExtension;
import modules.admin.domain.Tag.FilterAction;

public class UploadTagCriteria extends UploadAction<TagExtension> {
	@Override
	public TagExtension upload(TagExtension tag,
								Upload upload,
								UploadException exception,
								WebContext webContext)
	throws Exception {
		if (! upload.getFileName().endsWith(MimeType.xlsx.getStandardFileSuffix())) {
			exception.addError(new Problem("Only " + MimeType.xlsx.name() + " files are supported", null));
			throw exception;
		}

		try (InputStream is = upload.getInputStream()) {
			POISheetLoader loader = new POISheetLoader(is, 0, tag.getUploadModuleName(), tag.getUploadDocumentName(), exception);
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
			for (Bean bean : beansToTag) {
	
				if (FilterAction.tagRecordsThatMatch.equals(tag.getFilterAction())) {
					EXT.getTagManager().tag(tag.getBizId(), bean);
				} else if (FilterAction.unTagRecordsThatMatch.equals(tag.getFilterAction())) {
					// remove the tagged record
					EXT.getTagManager().untag(tag.getBizId(), bean);
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
}
