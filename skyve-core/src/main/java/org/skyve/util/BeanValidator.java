package org.skyve.util;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.MessageException;
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
	 * @param e The exception to populate.
	 */
	public static void validateBeanAgainstDocument(Document document, Bean bean) {
		ValidationUtil.validateBeanAgainstDocument(document, bean);
	}

	/**
	 * Validate a document instance against its metadata.
	 * 
	 * @param bean The bean to validate.
	 * @param e The exception to populate.
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
	 * Validate a bean against its bizlet .validate().
	 * NB This validation method does NOT recursively validate using bizlets through
	 * the base document hierarchy as the bizlet class should be arranged in such a way as to extend the
	 * bizlet methods required of the base bizlet classes through the standard java extension mechanism.
	 * 
	 * @param bizlet	To validate against
	 * @param bean	To validate
	 */
	public static <T extends Bean> void validateBeanAgainstBizlet(Bizlet<T> bizlet, T bean) {
		ValidationUtil.validateBeanAgainstBizlet(bizlet, bean);
	}

	
	/**
	 * Validate a bean against its bizlet .validate().
	 * NB This validation method does NOT recursively validate using bizlets through
	 * the base document hierarchy as the bizlet class should be arranged in such a way as to extend the
	 * bizlet methods required of the base bizlet classes through the standard java extension mechanism.
	 * 
	 * @param document	To get the bizlet to validate against
	 * @param bean	To validate
	 */
	public static <T extends Bean> void validateBeanAgainstBizlet(Document document, T bean) {
		Bizlet<T> bizlet = ((DocumentImpl) document).getBizlet(CORE.getUser().getCustomer());
		if (bizlet != null) { // has a bizlet
			validateBeanAgainstBizlet(bizlet, bean);
		}
	}
	
	/**
	 * Validate a bean against its bizlet .validate().
	 * NB This validation method does NOT recursively validate using bizlets through
	 * the base document hierarchy as the bizlet class should be arranged in such a way as to extend the
	 * bizlet methods required of the base bizlet classes through the standard java extension mechanism.
	 * 
	 * @param bean	To validate
	 */
	public static <T extends Bean> void validateBeanAgainstBizlet(T bean) {
		Customer c = CORE.getUser().getCustomer();
		Module m = c.getModule(bean.getBizModule());
		Document d = m.getDocument(c, bean.getBizDocument());
		validateBeanAgainstBizlet(d, bean);
	}

	/**
	 * Updates the bindings in the error message 
	 * based on the binding of the validatedBean in relation to the masterBean.
	 * 
	 * @param m
	 * @param masterBean
	 * @param validatedBean
	 */
	public static void processMessageBindings(Message m,
												Bean masterBean,
												Bean validatedBean) {
		ValidationUtil.processMessageBindings(CORE.getUser().getCustomer(), m, masterBean, validatedBean);
	}

	/**
	 * Updates the bindings in the messages in the exception 
	 * based on the binding of the validatedBean in relation to the masterBean.
	 * 
	 * @param m
	 * @param masterBean
	 * @param validatedBean
	 */
	public static void processMessageBindings(MessageException e,
												Bean masterBean,
												Bean validatedBean) {
		Customer c = CORE.getUser().getCustomer();
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
	 * 
	 * @param customer
	 * @param document
	 * @param bean
	 */
	public static void checkCollectionUniqueConstraints(Customer customer, Document document, Bean bean) {
		ValidationUtil.checkCollectionUniqueConstraints(customer, document, bean);
	}
	
	/**
	 * 
	 * @param customer
	 * @param bean
	 */
	public static void checkCollectionUniqueConstraints(Customer customer, Bean bean) {
		Module m = customer.getModule(bean.getBizModule());
		Document d = m.getDocument(customer, bean.getBizDocument());
		checkCollectionUniqueConstraints(customer, d, bean);
	}

	/**
	 * 
	 * @param document
	 * @param bean
	 */
	public static void checkCollectionUniqueConstraints(Document document, Bean bean) {
		checkCollectionUniqueConstraints(CORE.getUser().getCustomer(), document, bean);
	}
	
	/**
	 * 
	 * @param document
	 * @param bean
	 */
	public static void checkCollectionUniqueConstraints(Bean bean) {
		checkCollectionUniqueConstraints(CORE.getUser().getCustomer(), bean);
	}
}
