package modules.admin.Tag.actions;

import org.skyve.CORE;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

import modules.admin.Tag.TagExtension;

public class Clear implements ServerSideAction<TagExtension> {
	/**
	 * Clear tagged values.
	 */
	@Override
	public ServerSideActionResult<TagExtension> execute(TagExtension bean, WebContext webContext)
	throws Exception {
		//clear tagged values
		StringBuilder deleteSQL = new StringBuilder();
		deleteSQL.append("delete from ADM_Tagged where tag_id = ");
		deleteSQL.append("'").append(bean.getBizId()).append("'");
		deleteSQL.append(" and bizCustomer = '").append(bean.getBizCustomer()).append("'");
		
		Persistence persistence = CORE.getPersistence();
		persistence.newSQL(deleteSQL.toString()).execute();
		
		bean.setUploadTagged(Long.valueOf(0));
		bean.setTotalTagged(Long.valueOf(0));
		
		return new ServerSideActionResult<>(bean);
	}
}
