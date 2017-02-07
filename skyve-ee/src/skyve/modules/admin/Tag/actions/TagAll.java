package modules.admin.Tag.actions;

import java.util.List;

import modules.admin.domain.Tag;
import modules.admin.domain.Tagged;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

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
		for(Bean b: beans){
			//add bean to tagged
			Tagged tagged = Tagged.newInstance();
			tagged.setTag(bean);
			tagged.setTaggedModule(bean.getUploadModuleName());
			tagged.setTaggedDocument(bean.getUploadDocumentName());
			tagged.setTaggedBizId(b.getBizId());
			
			pers.upsertBeanTuple(tagged);
		}
		
		return new ServerSideActionResult<>(bean);
	}
}
