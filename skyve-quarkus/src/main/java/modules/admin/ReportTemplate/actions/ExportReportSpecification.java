package modules.admin.ReportTemplate.actions;

import org.skyve.CORE;
import org.skyve.content.MimeType;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Binder;
import org.skyve.util.JSON;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import modules.admin.domain.ReportTemplate;

/**
 * Download the report specification as JSON
 * 
 * @author RBB
 *
 */
public class ExportReportSpecification extends DownloadAction<ReportTemplate> {
	private String json = null;

	@Override
	public void prepare(ReportTemplate bean, WebContext webContext) throws Exception {
		// nothing to do here
	}

	@Override
	public Download download(ReportTemplate bean, WebContext webContext) throws Exception {

		// clone the report so that the unique bizId of the current bean won't be overwritten if the resulting JSON is uploaded again
		// so download, then upload should create a copy, not a replacement of the original
		ReportTemplate copy = Util.cloneToTransientBySerialization(bean);
		
		//clear all transient attributes
		Module module = CORE.getCustomer().getModule(ReportTemplate.MODULE_NAME);
		Document document  = module.getDocument(CORE.getCustomer(), ReportTemplate.DOCUMENT_NAME);
		for(Attribute a: document.getAttributes()) {
			if(!a.isPersistent()) {
				Binder.set(copy, a.getName(), null);
			}
		}
		
		json = JSON.marshall(CORE.getCustomer(), copy);
		
		return new Download(String.format("%s.json", bean.getName()), json, MimeType.json);
	}

}
