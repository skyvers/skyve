/**
 * JAXB-annotated metadata classes for deserialising behaviour scripts from XML.
 *
 * <p>Behaviour scripts are defined in action XML files and Bizlet metadata. The classes
 * in this package represent the JAXB binding layer:
 * <ul>
 *   <li>{@code BizletMetaData} — top-level JAXB root for Bizlet class references,
 *       including override settings and declared action list.
 *   <li>{@code ActionMetaData} — top-level JAXB root for server-side action scripts;
 *       holds a list of {@code StatementMetaData} steps.
 * </ul>
 *
 * @see org.skyve.impl.metadata.repository.behaviour.statement
 * @see org.skyve.impl.metadata.behaviour
 */
package org.skyve.impl.metadata.repository.behaviour;
