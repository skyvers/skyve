package modules.admin.ReportParameter;

import org.skyve.CORE;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.converters.Converter;
import org.skyve.util.Binder;

import modules.admin.domain.ReportParameter;
import modules.admin.domain.ReportTemplate;

public class ReportParameterExtension extends ReportParameter {

	private static final long serialVersionUID = 4674099989961751605L;

	/**
	 * Calculated field to return the default value as a String, regardless of the parameter type selected.
	 */
	@Override
	public String getDefaultValueString() {
		if (getTextDefaultValue() != null) {
			return getTextDefaultValue();
		} else if (getDateDefaultValue() != null) {
			return Binder.getDisplay(CORE.getCustomer(), this, ReportParameter.dateDefaultValuePropertyName);
		} else if (getNumericalDefaultValue() != null) {
			return Binder.getDisplay(CORE.getCustomer(), this, ReportParameter.numericalDefaultValuePropertyName);
		}
		return super.getDefaultValueString();
	}

	/**
	 * Calculated field to return the formatted input value, or default value if not supplied.
	 */
	@Override
	public String getFormattedInputValue() {
		if (getReportInputValue() != null) {
			if (getType() == Type.date) {
				try {
					DateOnly date = CORE.getCustomer().getDefaultDateConverter().fromDisplayValue(getReportInputValue());
					return CORE.getCustomer().getDefaultDateConverter().toDisplayValue(date);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
			return Binder.getDisplay(CORE.getCustomer(), this, ReportParameter.reportInputValuePropertyName);
		} else if (getDefaultValueString() != null) {
			return getDefaultValueString();
		} else {
			return getTestValueString();
		}
	}

	/**
	 * Calculated field to return the test value as a String, regardless of the parameter type selected.
	 */
	@Override
	public String getTestValueString() {
		if (getTextTestValue() != null) {
			return getTextTestValue();
		} else if (getDateTestValue() != null) {
			return Binder.getDisplay(CORE.getCustomer(), this, ReportParameter.dateTestValuePropertyName);
		} else if (getNumericalTestValue() != null) {
			return Binder.getDisplay(CORE.getCustomer(), this, ReportParameter.numericalTestValuePropertyName);
		}
		return super.getTestValueString();
	}

	/**
	 * <p>
	 * Validates that this parameter is valid and able to be passed into a report. A parameter is invalid if:
	 * </p>
	 * <ul>
	 * <li>it is required, no value is supplied and there is no default value
	 * <li>it is optional, and default value is provided
	 * <li>the parameter is a date, and the supplied date string is invalid
	 * <li>the parameter is a number, and the supplied number string is invalid
	 * </ul>
	 * 
	 * @param index The index this parameter is within a {@link ReportTemplate} or {@link ReportConfiguration}, used to indicate
	 *        the correct report parameter if invalid
	 * @param e The {@link ValidationException} to add the error message to if this report is invalid
	 */
	public void validate(final ValidationException e, final int index) {
		if ((Boolean.TRUE.equals(this.getRequired()) && this.getReportInputValue() == null && this.getDefaultValueString() == null)) {
			e.getMessages().add(new Message(
					Binder.createCompoundBinding(Binder.createIndexedBinding(ReportTemplate.parametersPropertyName, index),
							ReportParameter.reportInputValuePropertyName),
					String.format("Value for parameter '%s' is required.", this.getName())));
		} else if (Type.date == this.getType() && this.getReportInputValue() != null) {
			Converter<DateOnly> dateConverter = CORE.getCustomer().getDefaultDateConverter();
			try {
				dateConverter.fromDisplayValue(this.getReportInputValue());
			} catch (@SuppressWarnings("unused") Exception ex) {
				String expectedDateFormat = dateConverter.getFormatPattern();
				e.getMessages().add(new Message(
						Binder.createCompoundBinding(Binder.createIndexedBinding(ReportTemplate.parametersPropertyName, index),
														ReportParameter.reportInputValuePropertyName),
						String.format("Could not parse date value '%s'. Expected a date in the format '%s'.",
										getReportInputValue(),
										expectedDateFormat)));
			}
		} else if (Type.integer == this.getType() && this.getReportInputValue() != null) {
			try {
				Integer.parseInt(this.getReportInputValue());
			} catch (@SuppressWarnings("unused") NumberFormatException nfe) {
				e.getMessages().add(new Message(
						Binder.createCompoundBinding(Binder.createIndexedBinding(ReportTemplate.parametersPropertyName, index),
								ReportParameter.reportInputValuePropertyName),
						String.format("Could not parse numerical value '%s'. Expected a whole number.",
								this.getReportInputValue())));
			}
		} else if (Type.longInteger == this.getType() && this.getReportInputValue() != null) {
			try {
				Long.parseLong(this.getReportInputValue());
			} catch (@SuppressWarnings("unused") NumberFormatException nfe) {
				e.getMessages().add(new Message(
						Binder.createCompoundBinding(Binder.createIndexedBinding(ReportTemplate.parametersPropertyName, index),
								ReportParameter.reportInputValuePropertyName),
						String.format("Could not parse numerical value '%s'. Expected a whole number.",
								this.getReportInputValue())));
			}
		}
	}

	/**
	 * Validates that this parameter is valid and able to be passed into a report during testing. A parameter
	 * is invalid if no test value is provided.
	 * 
	 * @param e The {@link ValidationException} to add the error message to if this report is invalid
	 */
	public void validateTest(ValidationException e) {
		if (Boolean.TRUE.equals(this.getRequired()) && this.getTestValueString() == null) {
			e.getMessages().add(new Message(String.format("A test value for parameter '%s' is required.", this.getName())));
		}
	}
}
