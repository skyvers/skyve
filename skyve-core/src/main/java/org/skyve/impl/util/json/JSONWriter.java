package org.skyve.impl.util.json;

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.text.CharacterIterator;
import java.text.StringCharacterIterator;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.io.WKTWriter;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.Enumeration;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.module.Module;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

// TODO Clean up exception handling in JSON stuff
public class JSONWriter {
	private StringBuilder buf = new StringBuilder();
	private Stack<Object> calls = new Stack<>();
	private Customer customer;

	public JSONWriter(@Nullable Customer customer) {
		this.customer = customer;
	}

	public @Nonnull String write(@Nullable Object object, @Nullable Set<String> propertyNames) {
		buf.setLength(0);
		value(object, propertyNames, true);
		return buf.toString();
	}

	public static @Nonnull String write(long n) {
		return String.valueOf(n);
	}

	public static @Nonnull String write(double d) {
		return String.valueOf(d);
	}

	public static @Nonnull String write(char c) {
		return "\"" + c + "\"";
	}

	public static @Nonnull String write(boolean b) {
		return String.valueOf(b);
	}

	private void value(@Nullable Object object, @Nullable Set<String> propertyNames, boolean topLevel) {
		if (object == null || cyclic(object)) {
			add("null");
		}
		else {
			calls.push(object);
			if (object instanceof Class<?>) {
				Class<?> type = (Class<?>) object;
				string(type.getName());
			}
			else if (object instanceof Boolean) {
				bool(((Boolean) object).booleanValue());
			}
			else if (object instanceof Number) {
				add(object);
			}
			else if (object instanceof Date) {
				string(object.toString());
			}
			else if (object instanceof String) {
				string(object);
			}
			else if (object instanceof Character) {
				string(object);
			}
			else if (object instanceof Enumeration) {
				string(((Enumeration) object).toCode());
			}
			else if (object instanceof Enum<?>) {
				string(object);
			}
			else if (object instanceof Map<?, ?>) {
				map((Map<?, ?>) object, propertyNames, false);
			}
			else if (object.getClass().isArray()) {
				array(object, propertyNames, topLevel);
			}
			else if (object instanceof Iterator<?>) {
				array((Iterator<?>) object, propertyNames, topLevel);
			}
			else if (object instanceof Iterable<?>) {
				array(((Iterable<?>) object).iterator(), propertyNames, topLevel);
			}
			// if we have properties (we are doing a list projection),
			// then use the bizId as the bean and don't embed the JSON object
			else if (object instanceof Bean) {
				if ((propertyNames != null) && (! topLevel)) {
					string(((Bean) object).getBizId());
				}
				else {
					document((Bean) object, propertyNames, false);
				}
			}
			else if (object instanceof OptimisticLock) {
				string(((OptimisticLock) object).toString());
			}
			else if (object instanceof Geometry) {
				string(new WKTWriter().write((Geometry) object));
			}
			else {
				bean(object, propertyNames, false);
			}
			calls.pop();
		}
	}

	private boolean cyclic(@Nonnull Object object) {
		return calls.contains(object);
	}

	private void bean(@Nonnull Object object, @Nullable Set<String> propertyNames, boolean topLevel) {
		boolean firstProperty = true;

		add("{");
		BeanInfo info;
		if (propertyNames == null) {
			add("class", object.getClass(), propertyNames, topLevel);
			firstProperty = false;
		}

		try {
			Class<?> type = object.getClass();
			info = Introspector.getBeanInfo(type);
			PropertyDescriptor[] props = info.getPropertyDescriptors();
			for (int i = 0; i < props.length; ++i) {
				PropertyDescriptor prop = props[i];
				String name = prop.getName();
				Method accessor = prop.getReadMethod();
				Method mutator = prop.getWriteMethod();
				if ((accessor != null) && // has read access
						// not the hierarchical bean's children property
						(! (HierarchicalBean.class.isAssignableFrom(type) && "children".equals(name))) &&
						((mutator != null) || // has write access
							// errorMessage property in ErrorMessage
							"errorMessage".equals(name) ||
							// or is a collection, iterator or iterable
							Collection.class.isAssignableFrom(prop.getPropertyType()) ||
							Iterator.class.equals(prop.getPropertyType()) || 
							Iterable.class.equals(prop.getPropertyType()))) {
					if (! accessor.canAccess(object)) {
						accessor.setAccessible(true);
					}
					Object value = accessor.invoke(object, (Object[]) null);
					if (! firstProperty) {
						add(',');
					}
					add(name, value, propertyNames, topLevel);
					firstProperty = false;
				}
			}
		}
		catch (IllegalAccessException iae) {
			iae.printStackTrace();
		}
		catch (InvocationTargetException ite) {
			ite.getCause().printStackTrace();
			ite.printStackTrace();
		}
		catch (IntrospectionException ie) {
			ie.printStackTrace();
		}
		add("}");
	}

	private void document(@Nonnull Bean bean, @Nullable Set<String> propertyNames, boolean topLevel) {
		if (customer == null) {
			throw new IllegalStateException("Marshalling a Skyve Bean requires a customer");
		}
		add("{");
		try {
			add(Bean.MODULE_KEY, bean.getBizModule(), propertyNames, topLevel);
			add(',');
			add(Bean.DOCUMENT_KEY, bean.getBizDocument(), propertyNames, topLevel);

			if (propertyNames == null) { // This is a real bean we are marshalling - (for edit view)
				Module module = customer.getModule(bean.getBizModule());
				Document document = module.getDocument(customer, bean.getBizDocument());

				for (Attribute attribute : document.getAllAttributes(customer)) {
					String name = attribute.getName();
					add(',');
					if (attribute instanceof Reference) {
						Object value = BindUtil.get(bean, name);
						add(name, value, propertyNames, topLevel);
					}
					else {
						// Ensure that the code is sent back for attributes with domain values
						if (attribute.getDomainType() != null) {
							Object value = BindUtil.get(bean, name);
							add(name, value, propertyNames, topLevel);
						}
						// ensure Booleans output true or false, not yes or no
						else if (AttributeType.bool.equals(attribute.getAttributeType())) {
							Object value = BindUtil.get(bean, name);
							add(name, value, propertyNames, topLevel);
						}
						else {
							String value = BindUtil.getDisplay(customer, bean, name);
							add(name, value, propertyNames, topLevel);
						}
					}
				}

				add(',');
				add(Bean.DOCUMENT_ID, bean.getBizId(), propertyNames, topLevel);
				add(',');
				add(Bean.CUSTOMER_NAME, bean.getBizCustomer(), propertyNames, topLevel);
				add(',');
				add(Bean.DATA_GROUP_ID, bean.getBizDataGroupId(), propertyNames, topLevel);
				add(',');
				add(Bean.USER_ID, bean.getBizUserId(), propertyNames, topLevel);

				if (bean instanceof ChildBean<?>) {
					ChildBean<?> childBean = (ChildBean<?>) bean;
					add(',');
					add(ChildBean.PARENT_NAME, childBean.getParent(), propertyNames, topLevel);
					add(',');
					add(Bean.ORDINAL_NAME, childBean.getBizOrdinal(), propertyNames, topLevel);
				}
				
				if (bean instanceof HierarchicalBean<?>) {
					HierarchicalBean<?> hierarchicalBean = (HierarchicalBean<?>) bean;
					add(',');
					add(HierarchicalBean.PARENT_ID, hierarchicalBean.getBizParentId(), propertyNames, topLevel);
				}

				if (bean instanceof AbstractPersistentBean) {
					PersistentBean persistentBean = (PersistentBean) bean;
					add(',');
					add(PersistentBean.VERSION_NAME, persistentBean.getBizVersion(), propertyNames, topLevel);
					add(',');
					add(PersistentBean.LOCK_NAME, persistentBean.getBizLock(), propertyNames, topLevel);
				}
			}
			else { // we are marshalling a DynamicBean (for List View)
				for (String name : propertyNames) {
					Object value = null;
					try {
						value = BindUtil.get(bean, name);
					}
					catch (@SuppressWarnings("unused") Exception e) {
						// do nothing - we try and get bogus properties from map beans in the list views - summary rows for instance
					}
					add(',');
					name = BindUtil.sanitiseBinding(name);
					add(name, value, propertyNames, topLevel);
				}
			}
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		add("}");
	}

	private void add(@Nonnull String name,
						@Nullable Object value,
						@Nullable Set<String> propertyNames,
						boolean topLevel) {
		add('"');
		add(name);
		add("\":");
		value(value, propertyNames, topLevel);
	}

	private void map(@Nonnull Map<?, ?> map, @Nullable Set<String> propertyNames, boolean topLevel) {
		add("{");
		Iterator<?> it = map.entrySet().iterator();
		while (it.hasNext()) {
			Map.Entry<?, ?> e = (Map.Entry<?, ?>) it.next();
			value(e.getKey(), propertyNames, topLevel);
			add(":");
			value(e.getValue(), propertyNames, topLevel);
			if (it.hasNext())
				add(',');
		}
		add("}");
	}

	private void array(@Nonnull Iterator<?> it, @Nullable Set<String> propertyNames, boolean topLevel) {
		add("[");
		while (it.hasNext()) {
			value(it.next(), propertyNames, topLevel);
			if (it.hasNext())
				add(",");
		}
		add("]");
	}

	private void array(@Nonnull Object object, @Nullable Set<String> propertyNames, boolean topLevel) {
		add("[");
		int length = Array.getLength(object);
		for (int i = 0; i < length; ++i) {
			value(Array.get(object, i), propertyNames, topLevel);
			if (i < length - 1)
				add(',');
		}
		add("]");
	}

	private void bool(boolean b) {
		add(b ? "true" : "false");
	}

	private void string(@Nonnull Object obj) {
		add('"');
		CharacterIterator it = new StringCharacterIterator(obj.toString());
		for (char c = it.first(); c != CharacterIterator.DONE; c = it.next()) {
			if (c == '"')
				add("\\\"");
			else if (c == '\\')
				add("\\\\");
			else if (c == '/')
				add("\\/");
			else if (c == '\b')
				add("\\b");
			else if (c == '\f')
				add("\\f");
			else if (c == '\n')
				add("\\n");
			else if (c == '\r')
				add("\\r");
			else if (c == '\t')
				add("\\t");
			else if (Character.isISOControl(c)) {
				unicode(c);
			}
			else {
				// we need to remove all characters with a high order byte that is not zero
				// so that ServletOutputStream does not barf
//				if ((c & 0xff00) == 0) { // high order byte must be zero
//					add(c);
//				}
				// There is no text output from skyve servlets generated with ServletOutputStream any more.
				// They all use PrintWriter which translates character encodings to UTF-8 for us.
				// So there is no need to do any of the above stuff - just add the character!!
				add(c);
			}
		}
		add('"');
	}

	private void add(@Nullable Object obj) {
		buf.append(obj);
	}

	private void add(char c) {
		buf.append(c);
	}

	static char[] hex = "0123456789ABCDEF".toCharArray();

	private void unicode(char c) {
		add("\\u");
		int n = c;
		for (int i = 0; i < 4; ++i) {
			int digit = (n & 0xf000) >> 12;
			add(hex[digit]);
			n <<= 4;
		}
	}
}
