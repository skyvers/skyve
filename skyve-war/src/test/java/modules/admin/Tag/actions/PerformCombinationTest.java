package modules.admin.Tag.actions;

import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;

import modules.admin.Tag.TagExtension;

@SuppressWarnings("static-method")
public class PerformCombinationTest {

	@Test
	void executeWithNullCombinationsOperatorThrowsValidationException() {
		PerformCombination action = new PerformCombination();
		TagExtension bean = new TagExtension();
		// combinationsOperator is null → throws before calling CDI tagService
		assertThrows(ValidationException.class, () -> action.execute(bean, null));
	}
}
