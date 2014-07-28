package org.skyve.wildcat.util;

import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.logging.Level;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.ErrorMessage;
import org.skyve.domain.messages.UniqueConstraintViolationException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.messages.ValidationMessage;
import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.UniqueConstraint;
import org.skyve.wildcat.bind.BindUtil;
import org.skyve.wildcat.metadata.model.document.field.ConvertableField;
import org.skyve.wildcat.metadata.model.document.field.Date;
import org.skyve.wildcat.metadata.model.document.field.DateTime;
import org.skyve.wildcat.metadata.model.document.field.Decimal10;
import org.skyve.wildcat.metadata.model.document.field.Decimal2;
import org.skyve.wildcat.metadata.model.document.field.Decimal5;
import org.skyve.wildcat.metadata.model.document.field.LongInteger;
import org.skyve.wildcat.metadata.model.document.field.Text;
import org.skyve.wildcat.metadata.model.document.field.TextFormat;
import org.skyve.wildcat.metadata.model.document.field.Time;
import org.skyve.wildcat.metadata.model.document.field.Timestamp;
import org.skyve.wildcat.metadata.model.document.field.validator.DateValidator;
import org.skyve.wildcat.metadata.model.document.field.validator.DecimalValidator;
import org.skyve.wildcat.metadata.model.document.field.validator.IntegerValidator;
import org.skyve.wildcat.metadata.model.document.field.validator.LongValidator;
import org.skyve.wildcat.metadata.model.document.field.validator.TextValidator;

public class ValidationUtil {
	private ValidationUtil() {
		// no implementation
	}

	/**
	 * Validate a document instance against its metadata.
	 * 
	 * @param document The document to validate.
	 * @param bean The bean to validate.
	 * @param e The exception to populate.
	 * @throws ValidationException Thrown if validation or access errors are encountered.
	 */
	public static void validateBeanAgainstDocument(Document document, Bean bean) 
	throws ValidationException, MetaDataException {
		validateBeanAgainstDocument(CORE.getUser().getCustomer(), document, bean);
	}

	private static void validateBeanAgainstDocument(Customer customer, Document document, Bean bean)
	throws ValidationException {
		ValidationException e = new ValidationException(new ValidationMessage("Problems have occurred with the " + document.getSingularAlias() + "."));

		for (Attribute attribute : document.getAttributes()) {
			if (! attribute.getName().equals(Bean.BIZ_KEY)) {
				validateBeanPropertyAgainstAttribute(customer, attribute, bean, e);
			}
		}

		if (! e.getSubordinates().isEmpty()) {
			throw e;
		}
	}

	/**
	 * Validate a document attribute against its metadata.
	 * 
	 * @param customer	The current customer for this thread
	 * @param attribute The attribute to validate.
	 * @param bean The bean to validate
	 * @param e The exception to populate
	 * @throws ValidationException Only thrown if an exception occurs getting a bean value, 
	 * 								otherwise messages are appended to the e argument.
	 */
	@SuppressWarnings("unchecked")
	private static void validateBeanPropertyAgainstAttribute(Customer customer, Attribute attribute, Bean bean, ValidationException e)
	throws ValidationException {
		String binding = attribute.getName();
		String displayName = attribute.getDisplayName();
		Converter<?> converter = (attribute instanceof ConvertableField) ? 
									((ConvertableField) attribute).getConverterForCustomer(customer) : 
									null;
		Object attributeValue = getAttributeValue(bean, binding);
		if (attribute.isRequired()) {
			if (attributeValue == null) {
				e.getSubordinates().add(new ValidationMessage(binding, displayName +  " is required."));
			}
		}

		if (converter != null) {
			validateFormat(converter.getFormat(), attributeValue, binding, displayName, e);
			@SuppressWarnings("rawtypes")
			Validator validator = converter.getValidator();
			if (validator != null) {
				validator.validate(attributeValue, binding, displayName, converter, e);
			}
		}
		
		if (attribute instanceof Collection) {
			Collection collection = (Collection) attribute;
			Integer minCardinality = collection.getMinCardinality();
			if (minCardinality != null) {
				int min = minCardinality.intValue();
				if (min > 0) {
					List<Bean> collectionValue = (List<Bean>) attributeValue;
					if ((collectionValue == null) || (collectionValue.size() < min)) {
						e.getSubordinates().add(new ValidationMessage(binding, 
																		"At least " + min + ' ' + collection.getDisplayName() +
																			((min == 1) ? 
																				" record is required." : 
																				" records are required.")));
					}
				}
			}
			Integer maxCardinality = collection.getMaxCardinality();
			if (maxCardinality != null) {
				int max = maxCardinality.intValue();
				List<Bean> collectionValue = (List<Bean>) attributeValue;
				if ((collectionValue != null) && (collectionValue.size() > max)) {
					e.getSubordinates().add(new ValidationMessage(binding, 
																	"No more than " + max + ' ' + collection.getDisplayName() +
																		((max == 1) ? 
																			" record is allowed." : 
																			" records are allowed.")));
				}
			}
		}
		else if (attribute instanceof Text) {
			Text text = (Text) attribute;
			int fieldLength = text.getLength();
			if (attributeValue instanceof String) {
				String stringValue = (String) attributeValue;
				if (stringValue.length() > fieldLength) {
					e.getSubordinates().add(new ValidationMessage(binding, 
																	displayName + " is longer than " + fieldLength + " characters."));
				}
				TextFormat format = text.getFormat();
				if (format != null) {
					validateFormat(format.getFormat(), stringValue, binding, displayName, e);
				}
				TextValidator validator = text.getValidator();
				if (validator != null) {
					validator.validate(stringValue, binding, displayName, (Converter<String>) converter, e);
				}
			}
		}
		else if (attribute instanceof org.skyve.wildcat.metadata.model.document.field.Integer) {
			org.skyve.wildcat.metadata.model.document.field.Integer integer = (org.skyve.wildcat.metadata.model.document.field.Integer) attribute;
			IntegerValidator validator = integer.getValidator();
			if (validator != null) {
				if (attributeValue instanceof Integer) {
					validator.validate((Integer) attributeValue, binding, displayName, (Converter<Integer>) converter, e);
				}
			}
		}
		else if (attribute instanceof LongInteger) {
			LongInteger integer = (LongInteger) attribute;
			LongValidator validator = integer.getValidator();
			if (validator != null) {
				if (attributeValue instanceof Long) {
					validator.validate((Long) attributeValue, binding, displayName, (Converter<Long>) converter, e);
				}
			}
		}
		else if (attribute instanceof Date) {
			Date date = (Date) attribute;
			DateValidator validator = date.getValidator();
			if (validator != null) {
				if (attributeValue instanceof java.util.Date) {
					validator.validate((java.util.Date) attributeValue,
										binding,
										displayName,
										(Converter<java.util.Date>) converter,
										e);
				}
			}
		}
		else if (attribute instanceof DateTime) {
			DateTime dateTime = (DateTime) attribute;
			DateValidator validator = dateTime.getValidator();
			if (validator != null) {
				if (attributeValue instanceof java.util.Date) {
					validator.validate((java.util.Date) attributeValue,
										binding,
										displayName,
										(Converter<java.util.Date>) converter,
										e);
				}
			}
		}
		else if (attribute instanceof Time) {
			Time time = (Time) attribute;
			DateValidator validator = time.getValidator();
			if (validator != null) {
				if (attributeValue instanceof java.util.Date) {
					validator.validate((java.util.Date) attributeValue,
										binding,
										displayName,
										(Converter<java.util.Date>) converter,
										e);
				}
			}
		}
		else if (attribute instanceof Timestamp) {
			Timestamp timestamp = (Timestamp) attribute;
			DateValidator validator = timestamp.getValidator();
			if (validator != null) {
				if (attributeValue instanceof java.util.Date) {
					validator.validate((java.util.Date) attributeValue,
										binding,
										displayName,
										(Converter<java.util.Date>) converter,
										e);
				}
			}
		}
		else if (attribute instanceof Decimal2) {
			Decimal2 decimal2 = (Decimal2) attribute;
			DecimalValidator validator = decimal2.getValidator();
			if (validator != null) {
				if (attributeValue instanceof org.skyve.domain.types.Decimal2) {
					validator.validate((org.skyve.domain.types.Decimal2) attributeValue,
										binding,
										displayName,
										(Converter<Decimal>) converter,
										e);
				}
			}
		}
		else if (attribute instanceof Decimal5) {
			Decimal5 decimal5 = (Decimal5) attribute;
			DecimalValidator validator = decimal5.getValidator();
			if (validator != null) {
				if (attributeValue instanceof org.skyve.domain.types.Decimal5) {
					validator.validate((org.skyve.domain.types.Decimal5) attributeValue,
										binding,
										displayName,
										(Converter<Decimal>) converter,
										e);
				}
			}
		}
		else if (attribute instanceof Decimal10) {
			Decimal10 decimal10 = (Decimal10) attribute;
			DecimalValidator validator = decimal10.getValidator();
			if (validator != null) {
				if (attributeValue instanceof org.skyve.domain.types.Decimal10) {
					validator.validate((org.skyve.domain.types.Decimal10) attributeValue,
										binding,
										displayName,
										(Converter<Decimal>) converter,
										e);
				}
			}
		}
	}

	private static void validateFormat(Format<?> format,
										Object value,
										String binding,
										String displayName,
										ValidationException e) {
		if (format != null) {
			try {
				@SuppressWarnings({"rawtypes", "unchecked"})
				String display = ((Format) format).toDisplayValue(value);
				format.fromDisplayValue(display);
			}
			catch (Exception e1) {
				e.getSubordinates().add(new ValidationMessage(binding, 
																displayName + " value " + value + " does not match the format " + format.getMask() + " (A = alphanumeric, L = alpha, # = numeric)"));
			}
		}
	}
	
	private static Object getAttributeValue(Bean bean, String binding) 
	throws ValidationException {
		Object result = null;
		try {
			result = BindUtil.get(bean, binding);
		}
		catch (Exception e) {
			e.printStackTrace();
			throw new ValidationException(new ValidationMessage(binding,
																	"You do not have access.  Please contact your Biz Hub administrator."));
		}

		return result;
	}

	public static <T extends Bean> void validateBeanAgainstBizlet(Bizlet<T> bizlet, T bean) 
	throws ValidationException {
		ValidationException e = new ValidationException(new ValidationMessage("Problems have occurred."));

		try {
			if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "validate", "Entering " + bizlet.getClass().getName() + ".validate: " + bean);
			bizlet.validate(bean, e);
			if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "validate", "Exiting " + bizlet.getClass().getName() + ".validate: " + bean);
		}
		catch (ValidationException ve) {
			// validation method has thrown a validation exception that is not the e parameter passed in
			if (ve != e) {
				List<ErrorMessage> subordinates = ve.getSubordinates();
				if (subordinates.isEmpty()) { // ve is just a lone exception, add it as a subordinate
					e.getSubordinates().add(ve);
				}
				else { // ve has subordinates - add them
					e.getSubordinates().addAll(ve.getSubordinates());
				}
			}
			else {
				throw e;
			}
		}
		catch (Exception ex) {
			ex.printStackTrace();
			e.getSubordinates().add(new ValidationMessage("An error occurred processing " + 
															bizlet.getClass().getName() +
															".validate() - See stack trace in log"));
		}

		if (! e.getSubordinates().isEmpty()) {
			throw e;
		}
	}

	/**
	 * Updates the bindings in the error message (and its children) 
	 * based on the binding of the validatedBean in relation to the masterBean.
	 * 
	 * @param customer
	 * @param e
	 * @param masterBean
	 * @param validatedBean
	 * @throws DomainException
	 * @throws MetaDataException
	 */
	public static void processErrorMessageBindings(Customer customer,
													final ErrorMessage e,
													Bean masterBean,
													final Bean validatedBean) 
	throws DomainException, MetaDataException {
		Document masterDocument = customer.getModule(masterBean.getBizModule()).getDocument(customer, 
																								masterBean.getBizDocument());

		// find this object within the beanBeingSaved
		new BeanVisitor() {
			@Override
			protected boolean accept(String binding,
							Document document,
							Document owningDocument,
							Reference owningReference,
							Bean bean,
							boolean visitingInheritedDocument)
			throws DomainException, MetaDataException {
				if (bean == validatedBean) {
					if (binding.length() > 0) {
						e.setBindingPrefix(binding + '.');
					}

					return false;
				}

				return true;
			}
		}.visit(masterDocument, masterBean, customer);
	}

	public static void checkCollectionUniqueConstraints(Customer customer, Document document, Bean bean)
	throws UniqueConstraintViolationException, ValidationException {
		try {
			for (String referenceName : document.getReferenceNames()) {
				Reference reference = document.getReferenceByName(referenceName);
				if (reference instanceof Collection) {
					Collection collection = (Collection) reference;
					for (UniqueConstraint constraint : collection.getUniqueConstraints()) {
						Set<String> uniqueValues = new TreeSet<>();

						@SuppressWarnings("unchecked")
						List<Bean> elements = (List<Bean>) BindUtil.get(bean, referenceName);
						for (int i = 0, l = elements.size(); i < l; i++) {
							Bean element = elements.get(i);
							StringBuilder sb = new StringBuilder(128);

							for (String fieldName : constraint.getFieldNames()) {
								sb.append(BindUtil.getDisplay(customer, element, fieldName)).append(';');
							}

							if (! uniqueValues.add(sb.toString())) { // already exists
								String message = null;
								try {
									message = BindUtil.formatMessage(customer, constraint.getMessage(), element);
								}
								catch (Exception ex) {
									ex.printStackTrace();
									message = "Unique Constraint Violation occurred on collection " + referenceName +
												" but could not display the unique constraint message for constraint " +
												constraint.getName();
								}

								UniqueConstraintViolationException ucve = new UniqueConstraintViolationException(constraint.getName(), message);
								ucve.addBinding(referenceName + '[' + i + ']');
								throw ucve;
							}
						}
					}
				}
			}
		}
		catch (UniqueConstraintViolationException ve) {
			throw ve;
		}
		catch (Exception ex) {
			ex.printStackTrace();
			throw new ValidationException(new ValidationMessage("An error occurred checking collection unique constraints. - See stack trace in log"));
		}
	}
}
