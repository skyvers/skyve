package org.skyve.nlp.cron.elementprovider.recurring;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class EveryMonthStepTest {

	@Test
	void testCanProvideMonthReturnsFalseWithNoSegments() {
		// no matches() called → segments empty → size != 3 → returns false
		EveryMonthStep provider = new EveryMonthStep();
		assertFalse(provider.canProvideMonth());
	}

	@Test
	void testGetMonthElementReturnsStarWithNoSegments() {
		// no matches() called → segments empty → size <= 2 → returns "*"
		EveryMonthStep provider = new EveryMonthStep();
		assertThat(provider.getMonthElement(), is("*"));
	}

}
