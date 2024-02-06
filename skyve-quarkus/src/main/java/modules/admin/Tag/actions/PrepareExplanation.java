package modules.admin.Tag.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.Tag.TagExtension;

public class PrepareExplanation implements ServerSideAction<TagExtension> {
	/**
	 * Describe the set operation currently configured.
	 */
	@Override
	public ServerSideActionResult<TagExtension> execute(TagExtension bean, WebContext webContext) throws Exception {

		StringBuilder ex = new StringBuilder(128);

		if (bean.getOperandTag() != null) {
			bean.setOperandTagCount(Long.valueOf(bean.getOperandTag().count()));

			if (bean.getCombinationsOperator() != null) {
				switch (bean.getCombinationsOperator()) {
				case union:
					ex.append("Add to ");
					ex.append(" Tag '").append(bean.getName()).append("'");
					ex.append(" all records in ");
					if (bean.getOperandTag() == null) {
						ex.append("the other Tag.");
					} else {
						ex.append(" Tag '").append(bean.getOperandTag().getName()).append("'.");
					}
					ex.append("<br/><br/>Note: results are distinct so may not be equal to the sum of both Tags.");
					break;
				case except:
					ex.append("Remove all records ");
					ex.append(" from Tag '").append(bean.getName()).append("'");
					ex.append(" that are in ");
					if (bean.getOperandTag() == null) {
						ex.append("the other Tag.");
					} else {
						ex.append("Tag '").append(bean.getOperandTag().getName()).append("'.");
					}
					break;
				case intersect:
					ex.append("Leave only the records in ");
					ex.append(" Tag '").append(bean.getName()).append("' ");
					ex.append(" that are in both Tags.");
					break;
				default:
					// nothing
				}
			} else {
				ex.append("Do nothing - you have not selected an operator.");
			}
		}

		bean.setCombinationExplanation(ex.toString());

		return new ServerSideActionResult<>(bean);
	}
}
