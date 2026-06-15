package modules.admin.Tag.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import java.lang.reflect.Field;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideActionResult;

import modules.admin.Tag.TagExtension;
import modules.admin.Tag.TagService;
import modules.admin.domain.Tag.CombinationsOperator;

@SuppressWarnings("static-method")
class PerformCombinationTest {

	@Test
	void executeWithNullCombinationsOperatorThrowsValidationException() {
		PerformCombination action = new PerformCombination();
		TagExtension bean = new TagExtension();
		// combinationsOperator is null → throws before calling CDI tagService
		assertThrows(ValidationException.class, () -> action.execute(bean, null));
	}

	@Test
	void executeWithMissingOperandThrowsValidationException() {
		PerformCombination action = new PerformCombination();
		TagExtension bean = new TagExtension();
		bean.setCombinationsOperator(CombinationsOperator.union);

		assertThrows(ValidationException.class, () -> action.execute(bean, null));
	}

	@Test
	void executeWithUnionDelegatesToTagService() throws Exception {
		assertDelegates(CombinationsOperator.union, "union");
	}

	@Test
	void executeWithExceptDelegatesToTagService() throws Exception {
		assertDelegates(CombinationsOperator.except, "except");
	}

	@Test
	void executeWithIntersectDelegatesToTagService() throws Exception {
		assertDelegates(CombinationsOperator.intersect, "intersect");
	}

	private static void assertDelegates(CombinationsOperator operator, String methodName) throws Exception {
		TagService tagService = mock(TagService.class);
		PerformCombination action = actionWithService(tagService);
		TagExtension bean = new TagExtension();
		TagExtension operand = new TagExtension();
		bean.setCombinationsOperator(operator);
		bean.setOperandTag(operand);

		ServerSideActionResult<TagExtension> result = action.execute(bean, null);

		assertThat(result.getBean(), is(bean));
		switch (methodName) {
			case "union":
				verify(tagService).union(bean, operand);
				break;
			case "except":
				verify(tagService).except(bean, operand);
				break;
			case "intersect":
				verify(tagService).intersect(bean, operand);
				break;
			default:
				throw new IllegalArgumentException(methodName);
		}
	}

	private static PerformCombination actionWithService(TagService service) {
		PerformCombination action = new PerformCombination();
		try {
			Field field = PerformCombination.class.getDeclaredField("tagService");
			field.setAccessible(true);
			field.set(action, service);
		} catch (ReflectiveOperationException e) {
			throw new IllegalStateException("Failed to inject TagService", e);
		}
		return action;
	}
}
