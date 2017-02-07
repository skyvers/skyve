package modules.admin.Tag.actions;

import modules.admin.Tag.TagBizlet;
import modules.admin.domain.Tag;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

public class PrepareExplanation implements ServerSideAction<Tag> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 2886341074753936987L;

	/**
	 * Update the payment batch details.
	 */
	@Override
	public ServerSideActionResult<Tag> execute(Tag bean, WebContext webContext) throws Exception {

		StringBuilder ex = new StringBuilder(128);

		if (bean.getCombinationsOperator() != null) {
			switch (bean.getCombinationsOperator()) {
			case union:
				ex.append("Add to ");
				ex.append(" Tag '").append(bean.getName()).append("'");
				ex.append(" all records in ");
				if(bean.getActionTag()==null){
					ex.append("the other Tag.");
				} else {
					ex.append(" Tag '").append(bean.getActionTag().getName()).append("'.");
				}
				ex.append("<br/><br/>Note: results are distinct so may not be equal to the sum of both Tags.");
				break;
			case except:
				ex.append("Remove all records ");
				ex.append(" from Tag '").append(bean.getName()).append("'");
				ex.append(" that are in ");
				if(bean.getActionTag()==null){
					ex.append("the other Tag.");
				} else {
					ex.append("Tag '").append(bean.getActionTag().getName()).append("'.");
				}
				break;
			case intersect:
				ex.append("Leave only the records in ");
				ex.append(" Tag '").append(bean.getName()).append("' ");
				ex.append(" that are in both Tags.");
				break;
			default:
				//nothing
			}
		} else {
			ex.append("Do nothing - you have not selected an operator.");
		}

		bean.setCombinationExplanation(ex.toString());
		
		bean.setActionTagCount(TagBizlet.getCount(bean.getActionTag()));

		return new ServerSideActionResult<>(bean);
	}
}
