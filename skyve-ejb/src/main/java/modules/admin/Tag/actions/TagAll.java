package modules.admin.Tag.actions;

import java.util.List;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

import modules.admin.Tag.TagExtension;

public class TagAll implements ServerSideAction<TagExtension> {
	/**
	 * Tag all records
	 */
	@Override
	public ServerSideActionResult<TagExtension> execute(TagExtension bean, WebContext webContext)
	throws Exception {
		
		Persistence pers = CORE.getPersistence();
		
		DocumentQuery q = pers.newDocumentQuery(bean.getUploadModuleName(), bean.getUploadDocumentName());
		
		List<Bean> beans = q.projectedResults();
		EXT.getTagManager().tag(bean.getBizId(), beans);
		
		bean.setUploadTagged(Long.valueOf(bean.countDocument(bean.getUploadModuleName(), bean.getUploadDocumentName())));
		bean.setTotalTagged(Long.valueOf(bean.count()));
		
		return new ServerSideActionResult<>(bean);
	}
}
