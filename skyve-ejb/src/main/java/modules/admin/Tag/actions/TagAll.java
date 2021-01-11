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

import modules.admin.Tag.TagBizlet;
import modules.admin.domain.Tag;

public class TagAll implements ServerSideAction<Tag> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 2886341074753936987L;

	/**
	 * Tag all records
	 */
	@Override
	public ServerSideActionResult<Tag> execute(Tag bean, WebContext webContext)
	throws Exception {
		
		Persistence pers = CORE.getPersistence();
		
		DocumentQuery q = pers.newDocumentQuery(bean.getUploadModuleName(), bean.getUploadDocumentName());
		
		List<Bean> beans = q.projectedResults();
		EXT.tag(bean.getBizId(), beans);
		
		bean.setUploadTagged(TagBizlet.getCountOfDocument(bean, bean.getUploadModuleName(), bean.getUploadDocumentName()));
		bean.setTotalTagged(TagBizlet.getCount(bean));
		
		return new ServerSideActionResult<>(bean);
	}
}
