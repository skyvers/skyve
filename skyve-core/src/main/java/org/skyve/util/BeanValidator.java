package org.skyve.util;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.MessageException;
import org.skyve.domain.messages.UniqueConstraintViolationException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.util.ValidationUtil;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

/**
 * 
 */
public class BeanValidator {
	public static final String VALIDATION_REQUIRED_KEY = "validation.required";
	public static final String VALIDATION_FORMAT_KEY = "validation.format";
	public static final String VALIDATION_COLLECTION_MIN_CARDINALITY_SINGULAR_KEY = "validation.collection.minCardinality.singular";
	public static final String VALIDATION_COLLECTION_MIN_CARDINALITY_PLURAL_KEY = "validation.collection.minCardinality.plural";
	public static final String VALIDATION_COLLECTION_MAX_CARDINALITY_SINGULAR_KEY = "validation.collection.maxCardinality.singular";
	public static final String VALIDATION_COLLECTION_MAX_CARDINALITY_PLURAL_KEY = "validation.collection.maxCardinality.plural";
	public static final String VALIDATION_LENGTH_KEY = "validation.length";
	public static final String VALIDATION_ACCESS_KEY = "validation.access";
	public static final String VALIDATION_RANGE_LESS_KEY = "validation.range.less";
	public static final String VALIDATION_RANGE_BEFORE_KEY = "validation.range.before";
	public static final String VALIDATION_RANGE_GREATER_KEY = "validation.range.greater";
	public static final String VALIDATION_RANGE_AFTER_KEY = "validation.range.after";
	public static final String VALIDATION_RANGE_BETWEEN_KEY = "validation.range.between";
	public static final String VALIDATION_PRECISION_KEY = "validation.precision";
	public static final String VALIDATION_TEXT_FORMAT = "validation.text.format";

	/**
	 * Disallow instantiation
	 */
	private BeanValidator() {
		// nothing to see here
	}
	
	/**
	 * Validate a document instance against its metadata.
	 * 
	 * @param document The document to validate.
	 * @param bean The bean to validate.
	 * @throws ValidationException where the bean fails validation against its metadata
	 */
	public static void validateBeanAgainstDocument(Document document, Bean bean) {
		ValidationUtil.validateBeanAgainstDocument(document, bean);
	}

	/**
	 * <p>
	 * Validate all the attributes of a document instance against its metadata.
	 * Note: this will not traverse associations, collections and inverses.
	 * </p>
	 * <p>
	 * For example, it will validate that an required association is set, but will
	 * not traverse and validate the associated document and its attributes as well.
	 * </p>
	 * 
	 * @param bean The bean to validate
	 * @throws ValidationException where the bean fails validation against its metadata
	 */
	public static void validateBeanAgainstDocument(Bean bean) {
		Customer c = CORE.getUser().getCustomer();
		Module m = c.getModule(bean.getBizModule());
		Document d = m.getDocument(c, bean.getBizDocument());
		validateBeanAgainstDocument(d, bean);
	}

	/**
	 * Validate a document attribute against its metadata.
	 * 
	 * @param user	The current user for this thread
	 * @param attribute The attribute to validate.
	 * @param bean The bean to validate
	 * @param e The exception to populate
	 */
	public static void validateBeanPropertyAgainstAttribute(User user, 
																Attribute attribute, 
																Bean bean, 
																ValidationException e) {
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, attribute, bean, e);
	}

	/**
	 * Validate a document attribute against its metadata.
	 * 
	 * @param attribute The attribute to validate.
	 * @param bean The bean to validate
	 * @param e The exception to populate
	 */
	public static void validateBeanPropertyAgainstAttribute(Attribute attribute, Bean bean, ValidationException e) {
		validateBeanPropertyAgainstAttribute(CORE.getUser(), attribute, bean, e);
	}
	
	/**
	 * Validate a document attribute against its metadata.
	 * 
	 * @param attributeName The name of the attribute to validate.
	 * @param bean The bean to validate
	 * @param e The exception to populate
	 */
	public static void validateBeanPropertyAgainstAttribute(String attributeName, Bean bean, ValidationException e) {
		User u = CORE.getUser();
		Customer c = u.getCustomer();
		Module m = c.getModule(bean.getBizModule());
		Document d = m.getDocument(c, bean.getBizDocument());
		Attribute a = d.getAttribute(attributeName);
		validateBeanPropertyAgainstAttribute(u, a, bean, e);
	}
	
	
	/**
	 * Validate a bean against the supplied bizlet <code>.validate()</code>.
	 * NB This validation method does NOT recursively validate using bizlets through
	 * the base document hierarchy as the bizlet class should be arranged in such a way as to extend the
	 * bizlet methods required of the base bizlet classes through the standard java extension mechanism.
	 * 
	 * @param bizlet The specific bizlet to validate against
	 * @param bean The bean to validate
	 * @throws ValidationException where the bean fails validation against the bizlet
	 */
	public static <T extends Bean> void validateBeanAgainstBizlet(Bizlet<T> bizlet, T bean) {
		ValidationUtil.validateBeanAgainstBizlet(bizlet, bean);
	}

	
	/**
	 * Validate a bean against the specified document's bizlet <code>.validate()</code>.
	 * NB This validation method does NOT recursively validate using bizlets through
	 * the base document hierarchy as the bizlet class should be arranged in such a way as to extend the
	 * bizlet methods required of the base bizlet classes through the standard java extension mechanism.
	 * 
	 * @param document The specific document to get the bizlet to validate against
	 * @param bean The bean to validate
	 * @throws ValidationException where the bean fails validation against the document's bizlet
	 */
	public static <T extends Bean> void validateBeanAgainstBizlet(Document document, T bean) {
		Bizlet<T> bizlet = ((DocumentImpl) document).getBizlet(CORE.getUser().getCustomer());
		if (bizlet != null) { // has a bizlet
			validateBeanAgainstBizlet(bizlet, bean);
		}
	}
	
	/**
	 * Validate a bean against its bizlet <code>.validate()</code>.
	 * NB This validation method does NOT recursively validate using bizlets through
	 * the base document hierarchy as the bizlet class should be arranged in such a way as to extend the
	 * bizlet methods required of the base bizlet classes through the standard java extension mechanism.
	 * 
	 * @param bean The bean to be validated against its Bizlet
	 * @throws ValidationException where the bean fails validation against its bizlet
	 */
	public static <T extends Bean> void validateBeanAgainstBizlet(T bean) {
		Customer c = CORE.getUser().getCustomer();
		Module m = c.getModule(bean.getBizModule());
		Document d = m.getDocument(c, bean.getBizDocument());
		validateBeanAgainstBizlet(d, bean);
	}

	/**
	 * Updates the bindings in the error message based on the binding of the
	 * <code>validatedBean</code> in relation to the <code>masterBean</code>.
	 * 
	 * @param m The message containing the bindings to be updated
	 * @param masterBean
	 * @param validatedBean
	 */
	public static void processMessageBindings(Message m,
												Bean masterBean,
												Bean validatedBean) {
		ValidationUtil.processMessageBindings(CORE.getCustomer(), m, masterBean, validatedBean);
	}

	/**
	 * Updates the bindings within the Messages in the exception based on the binding
	 * of the <code>validatedBean</code> in relation to the <code>masterBean</code>.
	 * 
	 * @param e The exception containing the messages to be updated
	 * @param masterBean
	 * @param validatedBean
	 */
	public static void processMessageBindings(MessageException e,
												Bean masterBean,
												Bean validatedBean) {
		Customer c = CORE.getCustomer();
		for (Message m : e.getMessages()) {
			ValidationUtil.processMessageBindings(c, m, masterBean, validatedBean);
		}
	}
	
	/**
	 * Prepends the bindings in the messages in the exception with the given prefix.
	 * 
	 * @param e
	 * @param bindingPrefix	Prefix to prepend (excluding the final '.')
	 */
	public static void processMessageBindings(MessageException e, String bindingPrefix) {
		String bindingPrefixWithDot = bindingPrefix + '.';
		for (Message m : e.getMessages()) {
			m.setBindingPrefix(bindingPrefixWithDot);
		}
	}

	/**
	 * For every <code>collection</code> defined in metadata for the given <code>bean</code>,
	 * check that all the values pass the defined unique constraints in all the collections.
	 * 
	 * @param customer The customer to validate the bean against
	 * @param document The document defining the collections to be validated
	 * @param bean The bean containing the collection records to be validated
	 * @throws UniqueConstraintViolationException if duplicate values are found to be violating
	 *         the defined constraints in any collection
	 */
	public static void checkCollectionUniqueConstraints(Customer customer, Document document, Bean bean) {
		ValidationUtil.checkCollectionUniqueConstraints(customer, document, bean);
	}
	
	/**
	 * For every <code>collection</code> defined in metadata for the given <code>bean</code>,
	 * check that all the values pass the defined unique constraints in all the collections.
	 * 
	 * @param customer The customer to validate the bean against
	 * @param bean The bean containing the collection records to be validated
	 * @throws UniqueConstraintViolationException if duplicate values are found to be violating
	 *         the defined constraints in any collection
	 */
	public static void checkCollectionUniqueConstraints(Customer customer, Bean bean) {
		Module m = customer.getModule(bean.getBizModule());
		Document d = m.getDocument(customer, bean.getBizDocument());
		checkCollectionUniqueConstraints(customer, d, bean);
	}

	/**
	 * For every <code>collection</code> defined in metadata for the given <code>bean</code>,
	 * check that all the values pass the defined unique constraints in all the collections.
	 * 
	 * @param document The document defining the collections to be validated
	 * @param bean The bean containing the collection records to be validated
	 * @throws UniqueConstraintViolationException if duplicate values are found to be violating
	 *         the defined constraints in any collection
	 */
	public static void checkCollectionUniqueConstraints(Document document, Bean bean) {
		checkCollectionUniqueConstraints(CORE.getUser().getCustomer(), document, bean);
	}
	
	/**
	 * For every <code>collection</code> defined in metadata for the given <code>bean</code>,
	 * check that all the values pass the defined unique constraints in all the collections.
	 * 
	 * @param bean The bean containing the collection records to be validated
	 * @throws UniqueConstraintViolationException if duplicate values are found to be violating
	 *         the defined constraints in any collection
	 */
	public static void checkCollectionUniqueConstraints(Bean bean) {
		checkCollectionUniqueConstraints(CORE.getUser().getCustomer(), bean);
	}
}
