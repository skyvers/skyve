package modules.admin.ReportTemplate.actions;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.Charset;

import org.skyve.CORE;
import org.skyve.content.MimeType;
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

	private static final long serialVersionUID = 6741549541099049576L;

	private String json = null;

	@Override
	public void prepare(ReportTemplate bean, WebContext webContext) throws Exception {
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
		
		// write the output string to an input stream
		InputStream inputStream = new ByteArrayInputStream(json.toString().getBytes(Charset.forName("UTF-8")));

		return new Download(String.format("%s.json", bean.getName()), inputStream, MimeType.json);
	}

}
