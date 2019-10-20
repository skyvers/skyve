package org.skyve.impl.util;

import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.logging.Level;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.UniqueConstraintViolationException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.field.ConvertableField;
import org.skyve.impl.metadata.model.document.field.Date;
import org.skyve.impl.metadata.model.document.field.DateTime;
import org.skyve.impl.metadata.model.document.field.Decimal10;
import org.skyve.impl.metadata.model.document.field.Decimal2;
import org.skyve.impl.metadata.model.document.field.Decimal5;
import org.skyve.impl.metadata.model.document.field.LongInteger;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.model.document.field.TextFormat;
import org.skyve.impl.metadata.model.document.field.Time;
import org.skyve.impl.metadata.model.document.field.Timestamp;
import org.skyve.impl.metadata.model.document.field.validator.DateValidator;
import org.skyve.impl.metadata.model.document.field.validator.DecimalValidator;
import org.skyve.impl.metadata.model.document.field.validator.IntegerValidator;
import org.skyve.impl.metadata.model.document.field.validator.LongValidator;
import org.skyve.impl.metadata.model.document.field.validator.TextValidator;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.model.document.UniqueConstraint;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.BeanVisitor;

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
	 */
	public static void validateBeanAgainstDocument(Document document, Bean bean) {
		validateBeanAgainstDocument(CORE.getUser(), document, bean);
	}

	private static void validateBeanAgainstDocument(User user, Document document, Bean bean) {
		Customer customer = user.getCustomer();
		ValidationException e = new ValidationException();
		for (Attribute attribute : document.getAttributes()) {
			validateBeanPropertyAgainstAttribute(user, attribute, bean, e);
		}

		Extends inherits = document.getExtends();
		Document baseDocument = document;
		while (inherits != null) {
			Module baseModule = customer.getModule(baseDocument.getOwningModuleName());
			baseDocument = baseModule.getDocument(customer, inherits.getDocumentName());
			for (Attribute attribute : baseDocument.getAttributes()) {
				validateBeanPropertyAgainstAttribute(user, attribute, bean, e);
			}
			inherits = baseDocument.getExtends();
		}

		if (! e.getMessages().isEmpty()) {
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
	 */
	@SuppressWarnings("unchecked")
	public static void validateBeanPropertyAgainstAttribute(User user, Attribute attribute, Bean bean, ValidationException e) {
		String binding = attribute.getName();
		AttributeType type = attribute.getAttributeType();
		if (AttributeType.inverseOne.equals(type) || 
				AttributeType.inverseMany.equals(type) ||
				binding.equals(Bean.BIZ_KEY)) {
			return;
		}

		String displayName = attribute.getDisplayName();
		Converter<?> converter = (attribute instanceof ConvertableField) ? 
									((ConvertableField) attribute).getConverterForCustomer(user.getCustomer()) : 
									null;
		Object attributeValue = getAttributeValue(bean, binding);
		if (attribute.isRequired()) {
			if (attributeValue == null) {
				e.getMessages().add(new Message(binding, displayName +  " is required."));
			}
		}

		if (converter != null) {
			if (attributeValue != null) {
				validateFormat(converter.getFormat(), attributeValue, binding, bean, displayName, e);
				@SuppressWarnings("rawtypes")
				Validator validator = converter.getValidator();
				if (validator != null) {
					validator.validate(user, attributeValue, binding, displayName, converter, e);
				}
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
						e.getMessages().add(new Message(binding, 
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
					e.getMessages().add(new Message(binding, 
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
					e.getMessages().add(new Message(binding, 
														displayName + " is longer than " + fieldLength + " characters."));
				}
				TextFormat format = text.getFormat();
				if (format != null) {
					validateFormat(format.getFormat(), stringValue, binding, bean, displayName, e);
				}
				TextValidator validator = text.getValidator();
				if (validator != null) {
					validator.validate(user, stringValue, binding, displayName, (Converter<String>) converter, e);
				}
			}
		}
		else if (attribute instanceof org.skyve.impl.metadata.model.document.field.Integer) {
			org.skyve.impl.metadata.model.document.field.Integer integer = (org.skyve.impl.metadata.model.document.field.Integer) attribute;
			IntegerValidator validator = integer.getValidator();
			if (validator != null) {
				if (attributeValue instanceof Integer) {
					validator.validate(user, (Integer) attributeValue, binding, displayName, (Converter<Integer>) converter, e);
				}
			}
		}
		else if (attribute instanceof LongInteger) {
			LongInteger integer = (LongInteger) attribute;
			LongValidator validator = integer.getValidator();
			if (validator != null) {
				if (attributeValue instanceof Long) {
					validator.validate(user, (Long) attributeValue, binding, displayName, (Converter<Long>) converter, e);
				}
			}
		}
		else if (attribute instanceof Date) {
			Date date = (Date) attribute;
			DateValidator validator = date.getValidator();
			if (validator != null) {
				if (attributeValue instanceof java.util.Date) {
					validator.validate(user,
										(java.util.Date) attributeValue,
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
					validator.validate(user,
										(java.util.Date) attributeValue,
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
					validator.validate(user,
										(java.util.Date) attributeValue,
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
					validator.validate(user,
										(java.util.Date) attributeValue,
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
					validator.validate(user,
										(org.skyve.domain.types.Decimal2) attributeValue,
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
					validator.validate(user,
										(org.skyve.domain.types.Decimal5) attributeValue,
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
					validator.validate(user,
										(org.skyve.domain.types.Decimal10) attributeValue,
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
											Bean bean,
											String displayName,
											ValidationException e) {
		if (format != null) {
			try {
				@SuppressWarnings({"rawtypes", "unchecked"})
				String display = ((Format) format).toDisplayValue(value);
				Object newValue = format.fromDisplayValue(display);

				// ok, we've passed validation from the 2 calls above...
				// now, only set the newValue back on the bean if it was a reformatted string,
				// otherwise its another type that is just displayed a certain way
				if ((value instanceof String) && (newValue instanceof String)) {
					BindUtil.set(bean, binding, newValue);
				}
			}
			catch (@SuppressWarnings("unused") Exception e1) {
				e.getMessages().add(new Message(binding, 
													displayName + " value " + value + " does not match the format " + format.getMask() + " (A = alphanumeric, L = alpha, # = numeric)"));
			}
		}
	}
	
	private static Object getAttributeValue(Bean bean, String binding) {
		Object result = null;
		try {
			result = BindUtil.get(bean, binding);
		}
		catch (Exception e) {
			e.printStackTrace();
			throw new ValidationException(new Message(binding,
														"You do not have access.  Please contact your Biz Hub administrator."));
		}

		return result;
	}

	/**
	 * Validate a bean against its bizlet .validate().
	 * NB This validation method does NOT recursively validate using bizlets through
	 * the base document hierarchy as the bizlet class should be arranged in such a way as to extend the
	 * bizlet methods required of the base bizlet classes through the standard java extension mechanism.
	 * 
	 * @param bizlet	To validate against
	 * @param bean	To validate
	 */
	public static <T extends Bean> void validateBeanAgainstBizlet(Bizlet<T> bizlet, T bean) {
		ValidationException e = new ValidationException();

		try {
			CustomerImpl internalCustomer = (CustomerImpl) CORE.getUser().getCustomer();
			boolean vetoed = internalCustomer.interceptBeforeValidate(bean, e);
			if (! vetoed) {
				if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "validate", "Entering " + bizlet.getClass().getName() + ".validate: " + bean);
				bizlet.validate(bean, e);
				if (UtilImpl.BIZLET_TRACE) UtilImpl.LOGGER.logp(Level.INFO, bizlet.getClass().getName(), "validate", "Exiting " + bizlet.getClass().getName() + ".validate: " + bean);
				internalCustomer.interceptAfterValidate(bean, e);
			}
		}
		catch (ValidationException ve) {
			// validation method has thrown a validation exception that is not the e parameter passed in
			if (ve != e) {
				List<Message> messages = ve.getMessages();
				if (! messages.isEmpty()) { // add ve's messages
					e.getMessages().addAll(messages);
				}
			}
			else {
				throw e;
			}
		}
		catch (Exception ex) {
			ex.printStackTrace();
			e.getMessages().add(new Message("An error occurred processing " + 
												bizlet.getClass().getName() +
												".validate() - See stack trace in log"));
		}

		if (! e.getMessages().isEmpty()) {
			throw e;
		}
	}

	/**
	 * Updates the bindings in the message 
	 * based on the binding of the validatedBean in relation to the masterBean.
	 * 
	 * @param customer
	 * @param e
	 * @param masterBean
	 * @param validatedBean
	 */
	public static void processMessageBindings(Customer customer,
												final Message e,
												Bean masterBean,
												final Bean validatedBean) {
		Document masterDocument = customer.getModule(masterBean.getBizModule()).getDocument(customer, 
																								masterBean.getBizDocument());

		// find this object within the beanBeingSaved
		new BeanVisitor(false, true, false) {
			@Override
			protected boolean accept(String binding,
							Document document,
							Document owningDocument,
							Relation owningRelation,
							Bean bean) {
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

	public static void checkCollectionUniqueConstraints(Customer customer, Document document, Bean bean) {
		try {
			for (Attribute attribute : document.getAllAttributes()) {
				if (attribute instanceof Collection) {
					String referenceName = attribute.getName();
					Collection collection = (Collection) attribute;
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

								throw new UniqueConstraintViolationException(document,
																				constraint.getName(),
																				referenceName + '[' + i + ']',
																				message);
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
			throw new ValidationException(new Message("An error occurred checking collection unique constraints. - See stack trace in log"));
		}
	}
}
