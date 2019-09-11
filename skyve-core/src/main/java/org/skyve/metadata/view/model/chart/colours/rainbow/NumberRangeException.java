package org.skyve.metadata.view.model.chart.colours.rainbow;

public class NumberRangeException extends RainbowException {
	
	private static final long serialVersionUID = 4165381497766700805L;
	
	@SuppressWarnings("boxing")
	public NumberRangeException (double minNumber, double maxNumber) {
		super(String.format("maxNumber (%d) is not greater than minNumber (%d)", maxNumber, minNumber));
	}
}
