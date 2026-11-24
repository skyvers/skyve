package modules.kitchensink.LookupDescription;

import org.skyve.CORE;
import org.skyve.metadata.model.document.Bizlet;

import jakarta.annotation.Nonnull;
import modules.kitchensink.domain.KitchensinkDocument;
import modules.kitchensink.domain.LookupDescription;

/**
 * Numbers each lookup value when it is first saved, as an application numbers its invoices.
 * SAIL's {@code captureValue} reads the number back from the view.
 */
public class LookupDescriptionBizlet extends Bizlet<LookupDescription> {
	/**
	 * Sets the number from the {@code kitchensink.LookupDescription.number} sequence, if it has none.
	 *
	 * <p>Side effects: takes the next number of the sequence (an admin DocumentNumber record).
	 *
	 * @param bean the lookup value being saved
	 */
	@Override
	public void preSave(@Nonnull LookupDescription bean) throws Exception {
		if (bean.getNumber() == null) {
			bean.setNumber(CORE.getNumberGenerator().nextInt(KitchensinkDocument.LOOKUP_DESCRIPTION,
																LookupDescription.numberPropertyName));
		}
	}
}
