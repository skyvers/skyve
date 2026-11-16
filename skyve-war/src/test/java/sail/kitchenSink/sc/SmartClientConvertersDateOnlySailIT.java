package sail.kitchenSink.sc;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import util.AbstractInterpretedSailWorkflow;

/**
 * SmartClient on date time and timestamp attributes whose converters show only the date ({@code *_DateTime},
 * {@code *_Timestamp}): data entry, saved, and the list grid's filters by the day. SmartClient types such an attribute
 * as a date, so its edit form sends a date the server cannot read as a date time (a save with a value fails) and its
 * filter compares with midnight UTC of the typed day. PrimeFaces makes the records that SmartClient filters.
 * <p>
 * Disabled until that is fixed: the fix is waiting on a decision (see the converters decision log). The script runs
 * under Verify, so a run lists every failing step.
 * <p>
 * Runs against the server named by {@code -Dsail.baseUrl} (required), headless unless {@code -Dsail.headless=false}.
 */
class SmartClientConvertersDateOnlySailIT extends AbstractInterpretedSailWorkflow {
	@Test
	@Disabled("SmartClient types a date-only date time or timestamp as a date: fails until that is fixed")
	void dateOnly() {
		sailWorkflow("sail/kitchenSink/sc/sc-converters-dateOnly-pending.xml");
	}
}
