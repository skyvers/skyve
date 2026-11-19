package org.skyve.util;

import java.util.Set;

import org.skyve.impl.util.json.JSONReader;
import org.skyve.impl.util.json.JSONWriter;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.user.User;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Produces and consumes Skyve domain objects to and from JSON.
 *
 * <p>Marshaling delegates to {@link org.skyve.impl.util.json.JSONWriter} and can include
 * a property-name projection for query-result shaping.
 *
 * <p>Class attribute contract: complete ordinary Java beans and all Java records are
 * written with {@code "class"} as their first property. Projected ordinary Java beans
 * omit it, while records retain it. Skyve documents use leading {@code "bizModule"}
 * and {@code "bizDocument"} properties instead. Maps, collections, and scalar values
 * do not acquire class metadata of their own, although their nested values may have it.
 *
 * <p>Untyped unmarshaling interprets a leading {@code "class"} property as a Java
 * type discriminator and reconstructs that bean or record. A {@code "class"} property
 * in any other position is ordinary map data. Typed unmarshaling never uses payload
 * class metadata to select the target: the supplied {@link Class} is authoritative, so
 * record JSON may include or omit {@code "class"}.
 *
 * <p>User-aware unmarshaling reconstructs Skyve document instances wherever they
 * occur in the value graph. Other untyped values may be returned as Java objects,
 * dynamic maps/lists, or scalars depending on their metadata.
 *
 * <p>Thread-safe: each operation creates an independent reader or writer.
 */
public class JSON {
	/**
	 * Prevents instantiation of this static utility.
	 */
	private JSON() {
		// disallow instantiation
	}

	/**
	 * Produces Skyve JSON using customer metadata and an explicit property projection.
	 *
	 * <p>When {@code propertyNames} is non-null, ordinary Java beans omit the
	 * {@code "class"} property. Records always include it. When
	 * {@code propertyNames} is {@code null}, both ordinary Java beans and records include
	 * it. Skyve documents use {@code "bizModule"} and {@code "bizDocument"} instead.
	 *
	 * @param customer customer metadata used for Skyve documents, or {@code null} for plain values
	 * @param beanOrBeans root value, collection, document, record, or Java bean; may be {@code null}
	 * @param propertyNames projected query-result properties, or {@code null} for a complete value
	 * @return JSON text; never {@code null}
	 */
	public static final @Nonnull String marshall(@Nullable Customer customer,
													@Nullable Object beanOrBeans,
													@Nullable Set<String> propertyNames) {
		JSONWriter writer = new JSONWriter(customer);
		return writer.write(beanOrBeans, propertyNames);
	}

	/**
	 * Produces Skyve JSON using customer metadata for Skyve documents.
	 *
	 * <p>Complete ordinary Java beans and records include {@code "class"} as their first
	 * property. Skyve documents do not; they use {@code "bizModule"} and
	 * {@code "bizDocument"}.
	 *
	 * @param customer customer metadata; must not be {@code null}
	 * @param beanOrBeans root value, collection, document, record, or Java bean; may be {@code null}
	 * @return JSON text; never {@code null}
	 */
	public static final @Nonnull String marshall(@Nonnull Customer customer,
													@Nullable Object beanOrBeans) {
		return marshall(customer, beanOrBeans, null);
	}

	/**
	 * Produces complete non-document JSON without customer metadata.
	 *
	 * <p>Ordinary Java beans and records include {@code "class"} as their first property.
	 * Maps, collections, and scalar values do not add class metadata of their own.
	 *
	 * @param beanOrBeans root value, collection, record, or Java bean; may be {@code null}
	 * @return JSON text; never {@code null}
	 * @throws IllegalStateException if the graph contains a Skyve document
	 */
	public static final @Nonnull String marshall(@Nullable Object beanOrBeans) {
		return marshall(null, beanOrBeans, null);
	}

	/**
	 * Consumes JSON with a user context for Skyve document reconstruction.
	 *
	 * <p>A leading {@code "class"} property is consumed as a Java type discriminator.
	 * The named class is loaded and reconstructed. If {@code "class"} is not the first
	 * property, the containing object is returned as a dynamic map and the property is
	 * retained as ordinary data.
	 *
	 * <p>Side effects: may instantiate Skyve documents and payload-identified Java
	 * objects anywhere in the value graph.
	 *
	 * @param user current Skyve user, or {@code null} for no Skyve document reconstruction
	 * @param json JSON text containing one value; must not be {@code null}
	 * @return reconstructed object, dynamic map/list, scalar value, or {@code null}
	 * @throws Exception if parsing or object reconstruction fails
	 */
	public static final @Nullable Object unmarshall(@Nullable User user, @Nonnull String json) 
	throws Exception {
		JSONReader reader = new JSONReader(user);
		return reader.read(json);
	}

	/**
	 * Consumes JSON without a Skyve user context.
	 *
	 * <p>A leading {@code "class"} property is consumed as a Java type discriminator and
	 * the named ordinary Java bean or record is reconstructed. If it is not first, it is
	 * retained as ordinary data in a dynamic map. Objects beginning with Skyve document
	 * metadata remain dynamic maps because no customer metadata is available.
	 *
	 * @param json JSON text containing one value; must not be {@code null}
	 * @return reconstructed Java object, dynamic map/list, scalar value, or {@code null}
	 * @throws Exception if parsing or Java object reconstruction fails
	 */
	public static final @Nullable Object unmarshall(@Nonnull String json) 
	throws Exception {
		return unmarshall(null, json);
	}

	/**
	 * Consumes JSON as an explicitly supplied Java type without a Skyve user context.
	 *
	 * <p>This overload is intended for JSON that need not contain Skyve's
	 * {@code "class"} type discriminator. The supplied {@code type} is authoritative and
	 * controls conversion. If the JSON does contain a {@code "class"} property, its value
	 * is ignored rather than used to select or load a class. Record JSON may therefore
	 * include or omit {@code "class"}. Skyve document metadata remains a dynamic map
	 * because no user context is available.
	 *
	 * @param <T> target type
	 * @param json JSON text
	 * @param type target Java type
	 * @return the consumed value, or {@code null} when the JSON value is null
	 * @throws Exception when the JSON is malformed or cannot be converted to the
	 *         target type
	 * @since 10.0
	 */
	public static final @Nullable <T> T unmarshall(@Nonnull String json, @Nonnull Class<T> type) throws Exception {
		JSONReader reader = new JSONReader(null);
		return reader.read(json, type);
	}

	/**
	 * Consumes JSON as an explicitly supplied Java type with a Skyve user context.
	 * The context allows document instances nested anywhere in the value graph to be
	 * reconstructed from their {@code bizModule} and {@code bizDocument} metadata.
	 *
	 * <p>This overload is intended for JSON that need not contain Skyve's
	 * {@code "class"} type discriminator. The supplied {@code type} is authoritative and
	 * controls conversion. If the JSON does contain a {@code "class"} property, its value
	 * is ignored rather than used to select or load a class. Record JSON may therefore
	 * include or omit {@code "class"}. Nested Skyve documents are still recognised from
	 * their leading document metadata.
	 *
	 * @param <T> target type
	 * @param user current Skyve user
	 * @param json JSON text
	 * @param type target Java type
	 * @return the consumed value, or {@code null} when the JSON value is null
	 * @throws Exception when the JSON is malformed or cannot be converted
	 * @since 10.0
	 */
	public static final @Nullable <T> T unmarshall(@Nonnull User user,
													@Nonnull String json,
													@Nonnull Class<T> type)
	throws Exception {
		JSONReader reader = new JSONReader(user);
		return reader.read(json, type);
	}
}
