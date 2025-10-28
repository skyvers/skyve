package modules.admin.Tag.actions;

import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import jakarta.inject.Inject;
import modules.admin.Tag.TagExtension;
import modules.admin.Tag.TagService;
import modules.admin.domain.Tag;

/**
 * Server-side action that performs set operations between two tags.
 * Supports union (combine all items), except (remove operand items),
 * and intersect (keep only common items) operations.
 */
public class PerformCombination implements ServerSideAction<TagExtension> {
	@Inject
	private transient TagService tagService;

	/**
	 * Combine 2 tags with a set operator.
	 */
	@Override
	public ServerSideActionResult<TagExtension> execute(TagExtension bean, WebContext webContext) throws Exception {
		if (bean.getCombinationsOperator() == null) {
			throw new ValidationException(new Message(Tag.combinationsOperatorPropertyName, "You have not set an operator."));
		}

		if (bean.getOperandTag() == null) {
			throw new ValidationException(new Message(Tag.operandTagPropertyName, "You have not set the other Tag."));
		}

		switch (bean.getCombinationsOperator()) {
			case union:
				tagService.union(bean, bean.getOperandTag());
				break;
			case except:
				tagService.except(bean, bean.getOperandTag());
				break;
			case intersect:
				tagService.intersect(bean, bean.getOperandTag());
				break;
			default:
				break;
		}

		return new ServerSideActionResult<>(bean);
	}
}
