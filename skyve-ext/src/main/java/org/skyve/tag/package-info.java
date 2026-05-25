/**
 * Tag management API for user-defined bean groupings.
 *
 * <p>{@link org.skyve.tag.TagManager} is the service facade for creating, applying,
 * and querying tags. Obtain it via {@link org.skyve.EXT#getTagManager()}.
 *
 * <p>Tags are persisted as {@code admin.Tag} records. Any bean from any document can
 * be tagged, enabling cross-module selection sets, bulk operations, and set algebra
 * (intersect, union, subtract) between tag groups.
 *
 * @see org.skyve.tag.TagManager
 */
package org.skyve.tag;
