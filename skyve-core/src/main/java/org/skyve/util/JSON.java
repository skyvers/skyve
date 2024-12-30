package org.skyve.util;

import java.util.Set;

import org.skyve.impl.util.json.JSONReader;
import org.skyve.impl.util.json.JSONWriter;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.user.User;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

public class JSON {
	/**
	 * Create JSON.
	 * 
	 * @param customer
	 * @param beanOrBeans Either a Bean or List<Bean> or a Java Bean
	 * @param propertyNames Needed for marshalling the result of executing a Query.
	 * @return The JSON.
	 */
	public static final @Nonnull String marshall(@Nonnull Customer customer,
													@Nullable Object beanOrBeans,
													@Nonnull Set<String> propertyNames) {
		JSONWriter writer = new JSONWriter(customer);
		return writer.write(beanOrBeans, propertyNames);
	}

	/**
	 * Create JSON.
	 * 
	 * @param customer
	 * @param beanOrBeans Either a Bean or List<Bean> or a Java Bean
	 * @return The JSON.
	 */
	@SuppressWarnings("null") // call-through with nulls
	public static final @Nonnull String marshall(@Nonnull Customer customer,
													@Nullable Object beanOrBeans) {
		return marshall(customer, beanOrBeans, null);
	}

	/**
	 * Create JSON.
	 * 
	 * @param beanOrBeans Either a Bean or List<Bean> or a Java Bean
	 * @return The JSON.
	 */
	@SuppressWarnings("null") // call-through with nulls
	public static final @Nonnull String marshall(@Nullable Object beanOrBeans) {
		return marshall(null, beanOrBeans, null);
	}

	/**
	 * Consume JSON.
	 * 
	 * @param user
	 * @param json
	 * @return a Bean or List<Bean> or a Map or a Java bean.
	 */
	public static final @Nonnull Object unmarshall(@Nonnull User user, @Nonnull String json) 
	throws Exception {
		JSONReader reader = new JSONReader(user);
		return reader.read(json);
	}
	
	/**
	 * Consume JSON.
	 * 
	 * @param json
	 * @return a Bean or List<Bean> or a Map or a Java bean.
	 */
	public static final Object unmarshall(@Nonnull String json) 
	throws Exception {
		return unmarshall(null, json);
	}
}
