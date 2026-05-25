/**
 * JAXB-annotated metadata classes for deserialising module XML files.
 *
 * <p>A Skyve module is described by a {@code module.xml} file. The 41 classes in this
 * package form the full JAXB binding tree for that file, covering:
 * <ul>
 *   <li><b>Module root</b> — {@code ModuleMetaData} (maps to {@code <module>}), holding
 *       documents, queries, jobs, roles, and the module menu.
 *   <li><b>Menu items</b> — {@code ItemMetaData}, {@code GroupMetaData},
 *       {@code ListItemMetaData}, {@code EditItemMetaData}, {@code CalendarItemMetaData},
 *       {@code MapItemMetaData}, {@code TreeItemMetaData}, {@code LinkItemMetaData},
 *       and the container types used to organise them.
 *   <li><b>Queries</b> — {@code BizQLMetaData}, {@code SQLMetaData},
 *       {@code MetaDataQueryMetaData}, {@code BizQLReferenceMetaData},
 *       {@code SQLReferenceMetaData}, {@code MetaDataQueryReferenceMetaData} and
 *       associated column types.
 *   <li><b>Security</b> — {@code RoleMetaData}, {@code GroupMetaData},
 *       {@code DocumentPrivilegeMetaData}, {@code ActionPrivilegeMetaData},
 *       {@code ContentPermission}, {@code ContentRestriction}, {@code GrantedTo},
 *       {@code ApplicableTo}.
 *   <li><b>Actions</b> — {@code ActionMetaData} and supporting types.
 * </ul>
 *
 * <p>These classes are internal JAXB bindings; callers access module metadata through
 * {@link org.skyve.metadata.module.Module}.
 *
 * @see org.skyve.impl.metadata.module
 * @see org.skyve.metadata.module.Module
 */
package org.skyve.impl.metadata.repository.module;
