/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.jackrabbit.core;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Calendar;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import javax.jcr.AccessDeniedException;
import javax.jcr.InvalidItemStateException;
import javax.jcr.Item;
import javax.jcr.ItemExistsException;
import javax.jcr.ItemNotFoundException;
import javax.jcr.ItemVisitor;
import javax.jcr.MergeException;
import javax.jcr.NamespaceException;
import javax.jcr.NoSuchWorkspaceException;
import javax.jcr.Node;
import javax.jcr.NodeIterator;
import javax.jcr.PathNotFoundException;
import javax.jcr.Property;
import javax.jcr.PropertyIterator;
import javax.jcr.PropertyType;
import javax.jcr.RepositoryException;
import javax.jcr.UnsupportedRepositoryOperationException;
import javax.jcr.Value;
import javax.jcr.ValueFormatException;
import javax.jcr.lock.Lock;
import javax.jcr.lock.LockException;
import javax.jcr.nodetype.ConstraintViolationException;
import javax.jcr.nodetype.NoSuchNodeTypeException;
import javax.jcr.nodetype.NodeDefinition;
import javax.jcr.nodetype.NodeType;
import javax.jcr.nodetype.PropertyDefinition;
import javax.jcr.version.OnParentVersionAction;
import javax.jcr.version.Version;
import javax.jcr.version.VersionException;
import javax.jcr.version.VersionHistory;
import javax.jcr.version.VersionIterator;

import org.apache.jackrabbit.commons.iterator.NodeIteratorAdapter;
import org.apache.jackrabbit.commons.iterator.PropertyIteratorAdapter;
import org.apache.jackrabbit.core.lock.LockManager;
import org.apache.jackrabbit.core.nodetype.EffectiveNodeType;
import org.apache.jackrabbit.core.nodetype.ItemDef;
import org.apache.jackrabbit.core.nodetype.NodeDef;
import org.apache.jackrabbit.core.nodetype.NodeDefId;
import org.apache.jackrabbit.core.nodetype.NodeDefinitionImpl;
import org.apache.jackrabbit.core.nodetype.NodeTypeConflictException;
import org.apache.jackrabbit.core.nodetype.NodeTypeImpl;
import org.apache.jackrabbit.core.nodetype.NodeTypeManagerImpl;
import org.apache.jackrabbit.core.nodetype.NodeTypeRegistry;
import org.apache.jackrabbit.core.nodetype.PropDef;
import org.apache.jackrabbit.core.nodetype.PropertyDefinitionImpl;
import org.apache.jackrabbit.core.state.ItemState;
import org.apache.jackrabbit.core.state.ItemStateException;
import org.apache.jackrabbit.core.state.NodeReferences;
import org.apache.jackrabbit.core.state.NodeReferencesId;
import org.apache.jackrabbit.core.state.NodeState;
import org.apache.jackrabbit.core.state.PropertyState;
import org.apache.jackrabbit.core.value.InternalValue;
import org.apache.jackrabbit.core.version.DateVersionSelector;
import org.apache.jackrabbit.core.version.InternalFreeze;
import org.apache.jackrabbit.core.version.InternalFrozenNode;
import org.apache.jackrabbit.core.version.InternalFrozenVersionHistory;
import org.apache.jackrabbit.core.version.LabelVersionSelector;
import org.apache.jackrabbit.core.version.VersionImpl;
import org.apache.jackrabbit.core.version.VersionSelector;
import org.apache.jackrabbit.spi.Name;
import org.apache.jackrabbit.spi.Path;
import org.apache.jackrabbit.spi.commons.conversion.MalformedPathException;
import org.apache.jackrabbit.spi.commons.conversion.NameException;
import org.apache.jackrabbit.spi.commons.name.NameConstants;
import org.apache.jackrabbit.spi.commons.name.PathBuilder;
import org.apache.jackrabbit.spi.commons.name.PathFactoryImpl;
import org.apache.jackrabbit.util.ChildrenCollectorFilter;
import org.apache.jackrabbit.uuid.UUID;
import org.apache.jackrabbit.value.ValueHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <code>NodeImpl</code> implements the <code>Node</code> interface.
 */
public class NodeImpl extends ItemImpl implements Node {

    private static Logger log = LoggerFactory.getLogger(NodeImpl.class);

    /** same as ((NodeState) state).getNodeTypeName(); cached to avoid type casts */
    protected final Name primaryTypeName;

    /** the definition of this node */
    protected NodeDefinition definition;

    // flag set in status passed to getOrCreateProperty if property was created
    protected static final short CREATED = 0;

/* NEW CRAP */
    public synchronized Node addNodeWithUuid(
            String relPath, String nodeTypeName, String uuid)
            throws RepositoryException {
        // check state of this instance
        sanityCheck();

        NodeId id = null;
        if (uuid != null) {
            //if (!isNodeType(NameConstants.MIX_REFERENCEABLE)) {
            //    throw new UnsupportedRepositoryOperationException();
            //}
            // Test for existing UUID
            // @see SessionImporter.startNode(NodeInfo nodeInfo, List propInfos)
            try {
                session.getNodeByUUID(uuid);
                throw new ItemExistsException(
                    "A node with this UUID already exists: " + uuid);
            } catch (ItemNotFoundException infe) {
                id = new NodeId(new UUID(uuid));
            }
        }

        NodeType nt = null;
        if (nodeTypeName != null) {
            nt = session.getNodeTypeManager().getNodeType(nodeTypeName);
            if (nt.isMixin()) {
                throw new RepositoryException(
                    "Not a primary node type: " + nodeTypeName);
            }
        }

        return internalAddNode(relPath, (NodeTypeImpl) nt, id);
    }

/* NEW CRAP */
    
    
    /**
     * Protected constructor.
     *
     * @param itemMgr    the <code>ItemManager</code> that created this <code>Node</code> instance
     * @param session    the <code>Session</code> through which this <code>Node</code> is acquired
     * @param id         id of this <code>Node</code>
     * @param state      state associated with this <code>Node</code>
     * @param definition definition of <i>this</i> <code>Node</code>
     * @param listeners  listeners on life cylce changes of this <code>NodeImpl</code>
     */
    protected NodeImpl(ItemManager itemMgr, SessionImpl session, NodeId id,
                       NodeState state, NodeDefinition definition,
                       ItemLifeCycleListener[] listeners) {
        super(itemMgr, session, id, state, listeners);
        this.definition = definition;
        // paranoid sanity check
        NodeTypeRegistry ntReg = session.getNodeTypeManager().getNodeTypeRegistry();
        if (ntReg.isRegistered(state.getNodeTypeName())) {
            primaryTypeName = state.getNodeTypeName();
        } else {
            /**
             * todo need proper way of handling inconsistent/corrupt node type references
             * e.g. 'flag' nodes that refer to non-registered node types
             */
            log.warn("Fallback to nt:unstructured due to unknown node type '"
                    + state.getNodeTypeName() + "' of node " + safeGetJCRPath());
            primaryTypeName = NameConstants.NT_UNSTRUCTURED;
        }
    }

    /**
     * Returns the id of the property at <code>relPath</code> or <code>null</code>
     * if no property exists at <code>relPath</code>.
     * <p/>
     * Note that access rights are not checked.
     *
     * @param relPath relative path of a (possible) property
     * @return the id of the property at <code>relPath</code> or
     *         <code>null</code> if no property exists at <code>relPath</code>
     * @throws RepositoryException if <code>relPath</code> is not a valid
     *                             relative path
     */
    protected PropertyId resolveRelativePropertyPath(String relPath)
            throws RepositoryException {
        try {
            /**
             * first check if relPath is just a name (in which case we don't
             * have to build & resolve absolute path)
             */
            if (relPath.indexOf('/') == -1) {
                Name propName = session.getQName(relPath);
                // check if property entry exists
                NodeState thisState = (NodeState) state;
                if (thisState.hasPropertyName(propName)) {
                    return new PropertyId(thisState.getNodeId(), propName);
                } else {
                    // there's no property with that name
                    return null;
                }
            }
            /**
             * build and resolve absolute path
             */
            Path p = PathFactoryImpl.getInstance().create(
                    getPrimaryPath(), session.getQPath(relPath), true);
            return session.getHierarchyManager().resolvePropertyPath(p);
        } catch (NameException e) {
            String msg = "failed to resolve path " + relPath + " relative to " + safeGetJCRPath();
            log.debug(msg);
            throw new RepositoryException(msg, e);
        }
    }

    /**
     * Returns the id of the node at <code>relPath</code> or <code>null</code>
     * if no node exists at <code>relPath</code>.
     * <p/>
     * Note that access rights are not checked.
     *
     * @param relPath relative path of a (possible) node
     * @return the id of the node at <code>relPath</code> or
     *         <code>null</code> if no node exists at <code>relPath</code>
     * @throws RepositoryException if <code>relPath</code> is not a valid
     *                             relative path
     */
    protected NodeId resolveRelativeNodePath(String relPath)
            throws RepositoryException {
        try {
            /**
             * first check if relPath is just a name (in which case we don't
             * have to build & resolve absolute path)
             */
            Path p = session.getQPath(relPath);
            if (p.getLength() == 1) {
                Path.Element pe = p.getNameElement();
                if (pe.denotesName()) {
                    // check if node entry exists
                    NodeState thisState = (NodeState) state;
                    int index = pe.getIndex();
                    if (index == 0) {
                        index = 1;
                    }
                    NodeState.ChildNodeEntry cne =
                            thisState.getChildNodeEntry(pe.getName(), index);
                    if (cne != null) {
                        return cne.getId();
                    } else {
                        // there's no child node with that name
                        return null;
                    }
                }
            }
            /**
             * build and resolve absolute path
             */
            p = PathFactoryImpl.getInstance().create(getPrimaryPath(), p, true);
            return session.getHierarchyManager().resolveNodePath(p);
        } catch (NameException e) {
            String msg = "failed to resolve path " + relPath + " relative to " + safeGetJCRPath();
            log.debug(msg);
            throw new RepositoryException(msg, e);
        }
    }

    /**
     * Determines if there are pending unsaved changes either on <i>this</i>
     * node or on any node or property in the subtree below it.
     *
     * @return <code>true</code> if there are pending unsaved changes,
     *         <code>false</code> otherwise.
     * @throws RepositoryException if an error occured
     */
    protected boolean hasPendingChanges() throws RepositoryException {
        if (isTransient()) {
            return true;
        }
        Iterator iter = stateMgr.getDescendantTransientItemStates((NodeId) id);
        return iter.hasNext();
    }

    protected synchronized ItemState getOrCreateTransientItemState()
            throws RepositoryException {
        if (!isTransient()) {
            try {
                // make transient (copy-on-write)
                NodeState transientState =
                        stateMgr.createTransientNodeState((NodeState) state, ItemState.STATUS_EXISTING_MODIFIED);
                // replace persistent with transient state
                state = transientState;
            } catch (ItemStateException ise) {
                String msg = "failed to create transient state";
                log.debug(msg);
                throw new RepositoryException(msg, ise);
            }
        }
        return state;
    }

    /**
     * Computes the values of well-known system (i.e. protected) properties.
     * todo: duplicate code in BatchedItemOperations: consolidate and delegate to NodeTypeInstanceHandler
     *
     * @param name
     * @param def
     * @return
     * @throws RepositoryException
     */
    protected InternalValue[] computeSystemGeneratedPropertyValues(Name name,
                                                                   PropertyDefinitionImpl def)
            throws RepositoryException {
        InternalValue[] genValues = null;

        /**
         * todo: need to come up with some callback mechanism for applying system generated values
         * (e.g. using a NodeTypeInstanceHandler interface)
         */

        NodeState thisState = (NodeState) state;

        // compute system generated values
        NodeTypeImpl nt = (NodeTypeImpl) def.getDeclaringNodeType();
        if (nt.getQName().equals(NameConstants.MIX_REFERENCEABLE)) {
            // mix:referenceable node type
            if (name.equals(NameConstants.JCR_UUID)) {
                // jcr:uuid property
                genValues = new InternalValue[]{
                        InternalValue.create(thisState.getNodeId().getUUID().toString())
                };
            }
/*
       todo consolidate version history creation code (currently in ItemImpl.initVersionHistories)
       } else if (nt.getQName().equals(MIX_VERSIONABLE)) {
           // mix:versionable node type
           VersionHistory hist = session.getVersionManager().getOrCreateVersionHistory(this);
           if (name.equals(JCR_VERSIONHISTORY)) {
               // jcr:versionHistory property
               genValues = new InternalValue[]{InternalValue.create(new UUID(hist.getUUID()))};
           } else if (name.equals(JCR_BASEVERSION)) {
               // jcr:baseVersion property
               genValues = new InternalValue[]{InternalValue.create(new UUID(hist.getRootVersion().getUUID()))};
           } else if (name.equals(JCR_ISCHECKEDOUT)) {
               // jcr:isCheckedOut property
               genValues = new InternalValue[]{InternalValue.create(true)};
           } else if (name.equals(JCR_PREDECESSORS)) {
               // jcr:predecessors property
               genValues = new InternalValue[]{InternalValue.create(new UUID(hist.getRootVersion().getUUID()))};
           }
*/
        } else if (nt.getQName().equals(NameConstants.NT_HIERARCHYNODE)) {
            // nt:hierarchyNode node type
            if (name.equals(NameConstants.JCR_CREATED)) {
                // jcr:created property
                genValues = new InternalValue[]{InternalValue.create(Calendar.getInstance())};
            }
        } else if (nt.getQName().equals(NameConstants.NT_RESOURCE)) {
            // nt:resource node type
            if (name.equals(NameConstants.JCR_LASTMODIFIED)) {
                // jcr:lastModified property
                genValues = new InternalValue[]{InternalValue.create(Calendar.getInstance())};
            }
        } else if (nt.getQName().equals(NameConstants.NT_VERSION)) {
            // nt:version node type
            if (name.equals(NameConstants.JCR_CREATED)) {
                // jcr:created property
                genValues = new InternalValue[]{InternalValue.create(Calendar.getInstance())};
            }
        } else if (nt.getQName().equals(NameConstants.NT_BASE)) {
            // nt:base node type
            if (name.equals(NameConstants.JCR_PRIMARYTYPE)) {
                // jcr:primaryType property
                genValues = new InternalValue[]{InternalValue.create(primaryTypeName)};
            } else if (name.equals(NameConstants.JCR_MIXINTYPES)) {
                // jcr:mixinTypes property
                Set mixins = thisState.getMixinTypeNames();
                ArrayList values = new ArrayList(mixins.size());
                Iterator iter = mixins.iterator();
                while (iter.hasNext()) {
                    values.add(InternalValue.create((Name) iter.next()));
                }
                genValues = (InternalValue[]) values.toArray(new InternalValue[values.size()]);
            }
        }

        return genValues;
    }

    /**
     * @param name
     * @param type
     * @param multiValued
     * @param exactTypeMatch
     * @param status
     * @return
     * @throws ConstraintViolationException if no applicable property definition
     *                                      could be found
     * @throws RepositoryException          if another error occurs
     */
    protected PropertyImpl getOrCreateProperty(String name, int type,
                                               boolean multiValued,
                                               boolean exactTypeMatch,
                                               BitSet status)
            throws ConstraintViolationException, RepositoryException {
        try {
            return getOrCreateProperty(
                    session.getQName(name), type,
                    multiValued, exactTypeMatch, status);
        } catch (NameException e) {
            throw new RepositoryException("invalid property name: " + name, e);
        }
    }

    /**
     * @param name
     * @param type
     * @param multiValued
     * @param exactTypeMatch
     * @param status
     * @return
     * @throws ConstraintViolationException if no applicable property definition
     *                                      could be found
     * @throws RepositoryException          if another error occurs
     */
    protected synchronized PropertyImpl getOrCreateProperty(Name name, int type,
                                                            boolean multiValued,
                                                            boolean exactTypeMatch,
                                                            BitSet status)
            throws ConstraintViolationException, RepositoryException {
        status.clear();

        if (isNew() && !hasProperty(name)) {
            // this is a new node and the property does not exist yet
            // -> no need to check item manager
            PropertyDefinitionImpl def = getApplicablePropertyDefinition(
                    name, type, multiValued, exactTypeMatch);
            PropertyImpl prop = createChildProperty(name, type, def);
            status.set(CREATED);
            return prop;
        }


        /*
         * Please note, that this implementation does not win a price for beauty
         * or speed. It's never a good idea to use exceptions for semantical
         * control flow.
         * However, compared to the previous version, this one is thread save
         * and makes the test/get block atomic in respect to transactional
         * commits. the test/set can still fail.
         *
         * Old Version:

            NodeState thisState = (NodeState) state;
            if (thisState.hasPropertyName(name)) {
                /**
                 * the following call will throw ItemNotFoundException if the
                 * current session doesn't have read access
                 /
                return getProperty(name);
            }
            [...create block...]

        */
        try {
            PropertyId propId = new PropertyId(getNodeId(), name);
            return (PropertyImpl) itemMgr.getItem(propId);
        } catch (AccessDeniedException ade) {
            throw new ItemNotFoundException(name.toString());
        } catch (ItemNotFoundException e) {
            // does not exist yet:
            // find definition for the specified property and create property
            PropertyDefinitionImpl def = getApplicablePropertyDefinition(
                    name, type, multiValued, exactTypeMatch);
            PropertyImpl prop = createChildProperty(name, type, def);
            status.set(CREATED);
            return prop;
        }
    }

    protected synchronized PropertyImpl createChildProperty(Name name, int type,
                                                            PropertyDefinitionImpl def)
            throws RepositoryException {

        // create a new property state
        PropertyState propState;
        try {
            propState =
                    stateMgr.createTransientPropertyState(getNodeId(), name,
                            ItemState.STATUS_NEW);
            propState.setType(type);
            propState.setMultiValued(def.isMultiple());
            propState.setDefinitionId(def.unwrap().getId());
            // compute system generated values if necessary
            InternalValue[] genValues =
                    computeSystemGeneratedPropertyValues(name, def);
            InternalValue[] defValues = def.unwrap().getDefaultValues();
            if (genValues != null) {
                propState.setValues(genValues);
            } else if (defValues != null) {
                propState.setValues(defValues);
            }
        } catch (ItemStateException ise) {
            String msg = "failed to add property " + name + " to "
                    + safeGetJCRPath();
            log.debug(msg);
            throw new RepositoryException(msg, ise);
        }

        // create Property instance wrapping new property state
        PropertyImpl prop = itemMgr.createPropertyInstance(propState, def);

        // modify the state of 'this', i.e. the parent node
        NodeState thisState = (NodeState) getOrCreateTransientItemState();
        // add new property entry
        thisState.addPropertyName(name);

        return prop;
    }

    protected synchronized NodeImpl createChildNode(Name name,
                                                    NodeDefinitionImpl def,
                                                    NodeTypeImpl nodeType,
                                                    NodeId id)
            throws RepositoryException {
        // create a new node state
        NodeState nodeState;
        try {
            if (id == null) {
                id = new NodeId(UUID.randomUUID());
            }
            nodeState =
                    stateMgr.createTransientNodeState(id, nodeType.getQName(),
                            getNodeId(), ItemState.STATUS_NEW);
            nodeState.setDefinitionId(def.unwrap().getId());
        } catch (ItemStateException ise) {
            String msg = "failed to add child node " + name + " to "
                    + safeGetJCRPath();
            log.debug(msg);
            throw new RepositoryException(msg, ise);
        }

        // create Node instance wrapping new node state
        NodeImpl node;
        try {
            node = itemMgr.createNodeInstance(nodeState, def);
        } catch (RepositoryException re) {
            // something went wrong
            stateMgr.disposeTransientItemState(nodeState);
            // re-throw
            throw re;
        }

        // modify the state of 'this', i.e. the parent node
        NodeState thisState = (NodeState) getOrCreateTransientItemState();
        // add new child node entry
        thisState.addChildNodeEntry(name, nodeState.getNodeId());

        // add 'auto-create' properties defined in node type
        PropertyDefinition[] pda = nodeType.getAutoCreatedPropertyDefinitions();
        for (int i = 0; i < pda.length; i++) {
            PropertyDefinitionImpl pd = (PropertyDefinitionImpl) pda[i];
            node.createChildProperty(pd.getQName(), pd.getRequiredType(), pd);
        }

        // recursively add 'auto-create' child nodes defined in node type
        NodeDefinition[] nda = nodeType.getAutoCreatedNodeDefinitions();
        for (int i = 0; i < nda.length; i++) {
            NodeDefinitionImpl nd = (NodeDefinitionImpl) nda[i];
            node.createChildNode(nd.getQName(), nd,
                    (NodeTypeImpl) nd.getDefaultPrimaryType(), null);
        }

        return node;
    }

    protected void renameChildNode(Name oldName, int index, NodeId id,
                                   Name newName)
            throws RepositoryException {
        // modify the state of 'this', i.e. the parent node
        NodeState thisState = (NodeState) getOrCreateTransientItemState();
        thisState.renameChildNodeEntry(oldName, index, newName);
    }

    protected void removeChildProperty(String propName) throws RepositoryException {
        try {
            removeChildProperty(session.getQName(propName));
        } catch (NameException e) {
            throw new RepositoryException(
                    "invalid property name: " + propName, e);
        }
    }

    protected void removeChildProperty(Name propName) throws RepositoryException {
        // modify the state of 'this', i.e. the parent node
        NodeState thisState = (NodeState) getOrCreateTransientItemState();

        // remove the property entry
        if (!thisState.removePropertyName(propName)) {
            String msg = "failed to remove property " + propName + " of "
                    + safeGetJCRPath();
            log.debug(msg);
            throw new RepositoryException(msg);
        }

        // remove property
        PropertyId propId = new PropertyId(thisState.getNodeId(), propName);
        itemMgr.getItem(propId).setRemoved();
    }

    protected void removeChildNode(Name nodeName, int index)
            throws RepositoryException {
        // modify the state of 'this', i.e. the parent node
        NodeState thisState = (NodeState) getOrCreateTransientItemState();
        if (index == 0) {
            index = 1;
        }
        NodeState.ChildNodeEntry entry =
                thisState.getChildNodeEntry(nodeName, index);
        if (entry == null) {
            String msg = "failed to remove child " + nodeName + " of "
                    + safeGetJCRPath();
            log.debug(msg);
            throw new RepositoryException(msg);
        }

        // notify target of removal
        NodeId childId = entry.getId();
        NodeImpl childNode = (NodeImpl) itemMgr.getItem(childId);
        childNode.onRemove();

        // remove the child node entry
        if (!thisState.removeChildNodeEntry(nodeName, index)) {
            String msg = "failed to remove child " + nodeName + " of "
                    + safeGetJCRPath();
            log.debug(msg);
            throw new RepositoryException(msg);
        }
    }

    protected void onRedefine(NodeDefId defId) throws RepositoryException {
        NodeDefinitionImpl newDef =
                session.getNodeTypeManager().getNodeDefinition(defId);
        // modify the state of 'this', i.e. the target node
        NodeState thisState = (NodeState) getOrCreateTransientItemState();
        // set id of new definition
        thisState.setDefinitionId(defId);
        definition = newDef;
    }

    protected void onRemove() throws RepositoryException {
        // modify the state of 'this', i.e. the target node
        NodeState thisState = (NodeState) getOrCreateTransientItemState();

        if (thisState.hasChildNodeEntries()) {
            // remove child nodes
            // use temp array to avoid ConcurrentModificationException
            ArrayList tmp = new ArrayList(thisState.getChildNodeEntries());
            // remove from tail to avoid problems with same-name siblings
            for (int i = tmp.size() - 1; i >= 0; i--) {
                NodeState.ChildNodeEntry entry =
                        (NodeState.ChildNodeEntry) tmp.get(i);
                // recursively remove child node
                NodeId childId = entry.getId();
                NodeImpl childNode = (NodeImpl) itemMgr.getItem(childId);
                childNode.onRemove();
                // remove the child node entry
                thisState.removeChildNodeEntry(entry.getName(), entry.getIndex());
            }
        }

        // remove properties
        // use temp set to avoid ConcurrentModificationException
        HashSet tmp = new HashSet(thisState.getPropertyNames());
        for (Iterator iter = tmp.iterator(); iter.hasNext();) {
            Name propName = (Name) iter.next();
            // remove the property entry
            thisState.removePropertyName(propName);
            // remove property
            PropertyId propId = new PropertyId(thisState.getNodeId(), propName);
            itemMgr.getItem(propId).setRemoved();
        }

        // finally remove this node
        thisState.setParentId(null);
        itemMgr.getItem(id).setRemoved();
    }

    protected NodeImpl internalAddNode(String relPath, NodeTypeImpl nodeType)
            throws ItemExistsException, PathNotFoundException, VersionException,
            ConstraintViolationException, LockException, RepositoryException {
        return internalAddNode(relPath, nodeType, null);
    }

    protected NodeImpl internalAddNode(String relPath, NodeTypeImpl nodeType,
                                       NodeId id)
            throws ItemExistsException, PathNotFoundException, VersionException,
            ConstraintViolationException, LockException, RepositoryException {
        Path nodePath;
        Name nodeName;
        Path parentPath;
        try {
            nodePath =
                PathFactoryImpl.getInstance().create(getPrimaryPath(), session.getQPath(relPath), false)
                .getCanonicalPath();
            if (nodePath.getNameElement().getIndex() != 0) {
                String msg = "illegal subscript specified: " + nodePath;
                log.debug(msg);
                throw new RepositoryException(msg);
            }
            nodeName = nodePath.getNameElement().getName();
            parentPath = nodePath.getAncestor(1);
        } catch (NameException e) {
            String msg = "failed to resolve path " + relPath + " relative to "
                    + safeGetJCRPath();
            log.debug(msg);
            throw new RepositoryException(msg, e);
        }

        NodeImpl parentNode;
        try {
            Item parent = itemMgr.getItem(parentPath);
            if (!parent.isNode()) {
                String msg = "cannot add a node to property " + parentPath;
                log.debug(msg);
                throw new ConstraintViolationException(msg);
            }
            parentNode = (NodeImpl) parent;
        } catch (AccessDeniedException ade) {
            throw new PathNotFoundException(relPath);
        }

        // make sure that parent node is checked-out
        if (!parentNode.internalIsCheckedOut()) {
            String msg = safeGetJCRPath()
                    + ": cannot add a child to a checked-in node";
            log.debug(msg);
            throw new VersionException(msg);
        }

        // check lock status
        parentNode.checkLock();

        // delegate the creation of the child node to the parent node
        return parentNode.internalAddChildNode(nodeName, nodeType, id);
    }

    protected NodeImpl internalAddChildNode(Name nodeName,
                                            NodeTypeImpl nodeType)
            throws ItemExistsException, ConstraintViolationException,
            RepositoryException {
        return internalAddChildNode(nodeName, nodeType, null);
    }

    protected NodeImpl internalAddChildNode(Name nodeName,
                                            NodeTypeImpl nodeType, NodeId id)
            throws ItemExistsException, ConstraintViolationException,
            RepositoryException {
        Path nodePath;
        try {
            nodePath = PathFactoryImpl.getInstance().create(getPrimaryPath(), nodeName, true);
        } catch (MalformedPathException e) {
            // should never happen
            String msg = "internal error: invalid path " + safeGetJCRPath();
            log.debug(msg);
            throw new RepositoryException(msg, e);
        }

        NodeDefinitionImpl def;
        try {
            Name nodeTypeName = null;
            if (nodeType != null) {
                nodeTypeName = nodeType.getQName();
            }
            def = getApplicableChildNodeDefinition(nodeName, nodeTypeName);
        } catch (RepositoryException re) {
            String msg = "no definition found in parent node's node type for new node";
            log.debug(msg);
            throw new ConstraintViolationException(msg, re);
        }
        if (nodeType == null) {
            // use default node type
            nodeType = (NodeTypeImpl) def.getDefaultPrimaryType();
        }

        // check for name collisions
        NodeState thisState = (NodeState) state;
        NodeState.ChildNodeEntry cne = thisState.getChildNodeEntry(nodeName, 1);
        if (cne != null) {
            // there's already a child node entry with that name;
            // check same-name sibling setting of new node
            if (!def.allowsSameNameSiblings()) {
                throw new ItemExistsException(itemMgr.safeGetJCRPath(nodePath));
            }
            // check same-name sibling setting of existing node
            NodeId newId = cne.getId();
            if (!((NodeImpl) itemMgr.getItem(newId)).getDefinition().allowsSameNameSiblings()) {
                throw new ItemExistsException(itemMgr.safeGetJCRPath(nodePath));
            }
        }

        // check protected flag of parent (i.e. this) node
        if (definition.isProtected()) {
            String msg = safeGetJCRPath() + ": cannot add a child to a protected node";
            log.debug(msg);
            throw new ConstraintViolationException(msg);
        }

        // now do create the child node
        return createChildNode(nodeName, def, nodeType, id);
    }

    private void setMixinTypesProperty(Set mixinNames) throws RepositoryException {
        NodeState thisState = (NodeState) state;
        // get or create jcr:mixinTypes property
        PropertyImpl prop;
        if (thisState.hasPropertyName(NameConstants.JCR_MIXINTYPES)) {
            prop = (PropertyImpl) itemMgr.getItem(new PropertyId(thisState.getNodeId(), NameConstants.JCR_MIXINTYPES));
        } else {
            // find definition for the jcr:mixinTypes property and create property
            PropertyDefinitionImpl def = getApplicablePropertyDefinition(
                    NameConstants.JCR_MIXINTYPES, PropertyType.NAME, true, true);
            prop = createChildProperty(NameConstants.JCR_MIXINTYPES, PropertyType.NAME, def);
        }

        if (mixinNames.isEmpty()) {
            // purge empty jcr:mixinTypes property
            removeChildProperty(NameConstants.JCR_MIXINTYPES);
            return;
        }

        // call internalSetValue for setting the jcr:mixinTypes property
        // to avoid checking of the 'protected' flag
        InternalValue[] vals = new InternalValue[mixinNames.size()];
        Iterator iter = mixinNames.iterator();
        int cnt = 0;
        while (iter.hasNext()) {
            vals[cnt++] = InternalValue.create((Name) iter.next());
        }
        prop.internalSetValue(vals, PropertyType.NAME);
    }

    /**
     * Returns the <code>Name</code>s of this node's mixin types.
     *
     * @return a set of the <code>Name</code>s of this node's mixin types.
     */
    public Set getMixinTypeNames() {
        return ((NodeState) state).getMixinTypeNames();
    }

    /**
     * Returns the effective (i.e. merged and resolved) node type representation
     * of this node's primary and mixin node types.
     *
     * @return the effective node type
     * @throws RepositoryException if an error occurs
     */
    public EffectiveNodeType getEffectiveNodeType() throws RepositoryException {
        return getEffectiveNodeType(((NodeState) state).getMixinTypeNames());
    }

    /**
     * Small optimization to void double call for mixin types.
     *
     * @param mixins the set of mixins
     * @return the effective node type
     * @throws RepositoryException if an error occurs
     */
    private EffectiveNodeType getEffectiveNodeType(Set mixins)
            throws RepositoryException {

        // build effective node type of mixins & primary type
        NodeTypeRegistry ntReg = session.getNodeTypeManager().getNodeTypeRegistry();

        Name[] types = new Name[mixins.size() + 1];
        mixins.toArray(types);
        // primary type
        types[types.length - 1] = primaryTypeName;
        try {
            return ntReg.getEffectiveNodeType(types);
        } catch (NodeTypeConflictException ntce) {
            String msg = "internal error: failed to build effective node type for node "
                    + safeGetJCRPath();
            log.debug(msg);
            throw new RepositoryException(msg, ntce);
        }
    }

    /**
     * Returns the applicable child node definition for a child node with the
     * specified name and node type.
     *
     * @param nodeName
     * @param nodeTypeName
     * @return
     * @throws ConstraintViolationException if no applicable child node definition
     *                                      could be found
     * @throws RepositoryException          if another error occurs
     */
    protected NodeDefinitionImpl getApplicableChildNodeDefinition(Name nodeName,
                                                                  Name nodeTypeName)
            throws ConstraintViolationException, RepositoryException {
        NodeTypeManagerImpl ntMgr = session.getNodeTypeManager();
        NodeDef cnd = getEffectiveNodeType().getApplicableChildNodeDef(
                nodeName, nodeTypeName, ntMgr.getNodeTypeRegistry());
        return ntMgr.getNodeDefinition(cnd.getId());
    }

    /**
     * Returns the applicable property definition for a property with the
     * specified name and type.
     *
     * @param propertyName
     * @param type
     * @param multiValued
     * @param exactTypeMatch
     * @return
     * @throws ConstraintViolationException if no applicable property definition
     *                                      could be found
     * @throws RepositoryException          if another error occurs
     */
    protected PropertyDefinitionImpl getApplicablePropertyDefinition(Name propertyName,
                                                                     int type,
                                                                     boolean multiValued,
                                                                     boolean exactTypeMatch)
            throws ConstraintViolationException, RepositoryException {
        PropDef pd;
        if (exactTypeMatch || type == PropertyType.UNDEFINED) {
            pd = getEffectiveNodeType().getApplicablePropertyDef(
                    propertyName, type, multiValued);
        } else {
            try {
                // try to find a definition with matching type first
                pd = getEffectiveNodeType().getApplicablePropertyDef(
                        propertyName, type, multiValued);
            } catch (ConstraintViolationException cve) {
                // none found, now try by ignoring the type
                pd = getEffectiveNodeType().getApplicablePropertyDef(
                        propertyName, PropertyType.UNDEFINED, multiValued);
            }
        }
        return session.getNodeTypeManager().getPropertyDefinition(pd.getId());
    }

    protected void makePersistent() throws InvalidItemStateException {
        if (!isTransient()) {
            log.debug(safeGetJCRPath() + " (" + id + "): there's no transient state to persist");
            return;
        }

        NodeState transientState = (NodeState) state;

        NodeState persistentState = (NodeState) transientState.getOverlayedState();
        if (persistentState == null) {
            // this node is 'new'
            persistentState = stateMgr.createNew(transientState);
        }

        synchronized (persistentState) {
            // check staleness of transient state first
            if (transientState.isStale()) {
                String msg = safeGetJCRPath()
                        + ": the node cannot be saved because it has been modified externally.";
                log.debug(msg);
                throw new InvalidItemStateException(msg);
            }
            // copy state from transient state:
            // parent id's
            persistentState.setParentId(transientState.getParentId());
            // mixin types
            persistentState.setMixinTypeNames(transientState.getMixinTypeNames());
            // id of definition
            persistentState.setDefinitionId(transientState.getDefinitionId());
            // child node entries
            persistentState.setChildNodeEntries(transientState.getChildNodeEntries());
            // property entries
            persistentState.setPropertyNames(transientState.getPropertyNames());

            // make state persistent
            stateMgr.store(persistentState);
        }

        // tell state manager to disconnect item state
        stateMgr.disconnectTransientItemState(transientState);
        // swap transient state with persistent state
        state = persistentState;
        // reset status
        status = STATUS_NORMAL;
    }

    protected void restoreTransient(NodeState transientState)
            throws RepositoryException {
        NodeState thisState = (NodeState) getOrCreateTransientItemState();
        if (transientState.getStatus() == ItemState.STATUS_NEW
                && thisState.getStatus() != ItemState.STATUS_NEW) {
            thisState.setStatus(ItemState.STATUS_NEW);
            stateMgr.disconnectTransientItemState(thisState);
        }
        // reapply transient changes
        thisState.setParentId(transientState.getParentId());
        thisState.setMixinTypeNames(transientState.getMixinTypeNames());
        thisState.setDefinitionId(transientState.getDefinitionId());
        thisState.setChildNodeEntries(transientState.getChildNodeEntries());
        thisState.setPropertyNames(transientState.getPropertyNames());
    }

    /**
     * Same as {@link Node#addMixin(String)} except that it takes a
     * <code>Name</code> instead of a <code>String</code>.
     *
     * @see Node#addMixin(String)
     */
    public void addMixin(Name mixinName)
            throws NoSuchNodeTypeException, VersionException,
            ConstraintViolationException, LockException, RepositoryException {
        // check state of this instance
        sanityCheck();

        // make sure this node is checked-out
        if (!internalIsCheckedOut()) {
            String msg = safeGetJCRPath() + ": cannot add a mixin node type to a checked-in node";
            log.debug(msg);
            throw new VersionException(msg);
        }

        // check protected flag
        if (definition.isProtected()) {
            String msg = safeGetJCRPath() + ": cannot add a mixin node type to a protected node";
            log.debug(msg);
            throw new ConstraintViolationException(msg);
        }

        // check lock status
        checkLock();

        NodeTypeManagerImpl ntMgr = session.getNodeTypeManager();
        NodeTypeImpl mixin = ntMgr.getNodeType(mixinName);
        if (!mixin.isMixin()) {
            throw new RepositoryException(mixinName + ": not a mixin node type");
        }

        NodeTypeImpl primaryType = ntMgr.getNodeType(primaryTypeName);
        if (primaryType.isDerivedFrom(mixinName)) {
            // new mixin is already included in primary type
            return;
        }

        // build effective node type of mixin's & primary type in order to detect conflicts
        NodeTypeRegistry ntReg = ntMgr.getNodeTypeRegistry();
        EffectiveNodeType entExisting;
        try {
            // existing mixin's
            HashSet set = new HashSet(((NodeState) state).getMixinTypeNames());
            // primary type
            set.add(primaryTypeName);
            // build effective node type representing primary type including existing mixin's
            entExisting = ntReg.getEffectiveNodeType((Name[]) set.toArray(new Name[set.size()]));
            if (entExisting.includesNodeType(mixinName)) {
                // new mixin is already included in existing mixin type(s)
                return;
            }

            // add new mixin
            set.add(mixinName);
            // try to build new effective node type (will throw in case of conflicts)
            ntReg.getEffectiveNodeType((Name[]) set.toArray(new Name[set.size()]));
        } catch (NodeTypeConflictException ntce) {
            throw new ConstraintViolationException(ntce.getMessage());
        }

        // do the actual modifications implied by the new mixin;
        // try to revert the changes in case an exception occurs
        try {
            // modify the state of this node
            NodeState thisState = (NodeState) getOrCreateTransientItemState();
            // add mixin name
            Set mixins = new HashSet(thisState.getMixinTypeNames());
            mixins.add(mixinName);
            thisState.setMixinTypeNames(mixins);

            // set jcr:mixinTypes property
            setMixinTypesProperty(mixins);

            // add 'auto-create' properties defined in mixin type
            PropertyDefinition[] pda = mixin.getAutoCreatedPropertyDefinitions();
            for (int i = 0; i < pda.length; i++) {
                PropertyDefinitionImpl pd = (PropertyDefinitionImpl) pda[i];
                // make sure that the property is not already defined by primary type
                // or existing mixin's
                NodeTypeImpl declaringNT = (NodeTypeImpl) pd.getDeclaringNodeType();
                if (!entExisting.includesNodeType(declaringNT.getQName())) {
                    createChildProperty(pd.getQName(), pd.getRequiredType(), pd);
                }
            }

            // recursively add 'auto-create' child nodes defined in mixin type
            NodeDefinition[] nda = mixin.getAutoCreatedNodeDefinitions();
            for (int i = 0; i < nda.length; i++) {
                NodeDefinitionImpl nd = (NodeDefinitionImpl) nda[i];
                // make sure that the child node is not already defined by primary type
                // or existing mixin's
                NodeTypeImpl declaringNT = (NodeTypeImpl) nd.getDeclaringNodeType();
                if (!entExisting.includesNodeType(declaringNT.getQName())) {
                    createChildNode(nd.getQName(), nd, (NodeTypeImpl) nd.getDefaultPrimaryType(), null);
                }
            }
        } catch (RepositoryException re) {
            // try to undo the modifications by removing the mixin
            try {
                removeMixin(mixinName);
            } catch (RepositoryException re1) {
                // silently ignore & fall through
            }
            throw re;
        }
    }

    /**
     * Same as {@link Node#removeMixin(String)} except that it takes a
     * <code>Name</code> instead of a <code>String</code>.
     *
     * @see Node#removeMixin(String)
     */
    public void removeMixin(Name mixinName)
            throws NoSuchNodeTypeException, VersionException,
            ConstraintViolationException, LockException, RepositoryException {
        // check state of this instance
        sanityCheck();

        // make sure this node is checked-out
        if (!internalIsCheckedOut()) {
            String msg = safeGetJCRPath()
                    + ": cannot remove a mixin node type from a checked-in node";
            log.debug(msg);
            throw new VersionException(msg);
        }

        // check protected flag
        if (definition.isProtected()) {
            String msg = safeGetJCRPath()
                    + ": cannot remove a mixin node type from a protected node";
            log.debug(msg);
            throw new ConstraintViolationException(msg);
        }

        // check lock status
        checkLock();

        // check if mixin is assigned
        if (!((NodeState) state).getMixinTypeNames().contains(mixinName)) {
            throw new NoSuchNodeTypeException();
        }

        NodeTypeManagerImpl ntMgr = session.getNodeTypeManager();
        NodeTypeRegistry ntReg = ntMgr.getNodeTypeRegistry();

        // build effective node type of remaining mixin's & primary type
        Set remainingMixins = new HashSet(((NodeState) state).getMixinTypeNames());
        // remove name of target mixin
        remainingMixins.remove(mixinName);
        EffectiveNodeType entRemaining;
        try {
            // remaining mixin's
            HashSet set = new HashSet(remainingMixins);
            // primary type
            set.add(primaryTypeName);
            // build effective node type representing primary type including remaining mixin's
            entRemaining = ntReg.getEffectiveNodeType((Name[]) set.toArray(new Name[set.size()]));
        } catch (NodeTypeConflictException ntce) {
            throw new ConstraintViolationException(ntce.getMessage());
        }

        /**
         * mix:referenceable needs special handling because it has
         * special semantics:
         * it can only be removed if there no more references to this node
         */
        NodeTypeImpl mixin = ntMgr.getNodeType(mixinName);
        if ((NameConstants.MIX_REFERENCEABLE.equals(mixinName)
                || mixin.isDerivedFrom(NameConstants.MIX_REFERENCEABLE))
                && !entRemaining.includesNodeType(NameConstants.MIX_REFERENCEABLE)) {
            // removing this mixin would effectively remove mix:referenceable:
            // make sure no references exist
            PropertyIterator iter = getReferences();
            if (iter.hasNext()) {
                throw new ConstraintViolationException(mixinName + " can not be removed: the node is being referenced"
                        + " through at least one property of type REFERENCE");
            }
        }

        // modify the state of this node
        NodeState thisState = (NodeState) getOrCreateTransientItemState();
        thisState.setMixinTypeNames(remainingMixins);

        // set jcr:mixinTypes property
        setMixinTypesProperty(remainingMixins);

        // shortcut
        if (mixin.getChildNodeDefinitions().length == 0
                && mixin.getPropertyDefinitions().length == 0) {
            // the node type has neither property nor child node definitions,
            // i.e. we're done
            return;
        }

        // walk through properties and child nodes and remove those that have been
        // defined by the specified mixin type

        // use temp set to avoid ConcurrentModificationException
        HashSet set = new HashSet(thisState.getPropertyNames());
        for (Iterator iter = set.iterator(); iter.hasNext();) {
            Name propName = (Name) iter.next();
            PropertyImpl prop = (PropertyImpl) itemMgr.getItem(
                    new PropertyId(thisState.getNodeId(), propName));
            // check if property has been defined by mixin type (or one of its supertypes)
            NodeTypeImpl declaringNT = (NodeTypeImpl) prop.getDefinition().getDeclaringNodeType();
            if (!entRemaining.includesNodeType(declaringNT.getQName())) {
                // the remaining effective node type doesn't include the
                // node type that declared this property, it is thus safe
                // to remove it
                removeChildProperty(propName);
            }
        }
        // use temp array to avoid ConcurrentModificationException
        ArrayList list = new ArrayList(thisState.getChildNodeEntries());
        // start from tail to avoid problems with same-name siblings
        for (int i = list.size() - 1; i >= 0; i--) {
            NodeState.ChildNodeEntry entry = (NodeState.ChildNodeEntry) list.get(i);
            NodeImpl node = (NodeImpl) itemMgr.getItem(entry.getId());
            // check if node has been defined by mixin type (or one of its supertypes)
            NodeTypeImpl declaringNT = (NodeTypeImpl) node.getDefinition().getDeclaringNodeType();
            if (!entRemaining.includesNodeType(declaringNT.getQName())) {
                // the remaining effective node type doesn't include the
                // node type that declared this child node, it is thus safe
                // to remove it
                removeChildNode(entry.getName(), entry.getIndex());
            }
        }
    }

    /**
     * Same as {@link Node#isNodeType(String)} except that it takes a
     * <code>Name</code> instead of a <code>String</code>.
     *
     * @param ntName name of node type
     * @return <code>true</code> if this node is of the specified node type;
     *         otherwise <code>false</code>
     */
    public boolean isNodeType(Name ntName) throws RepositoryException {
        // check state of this instance
        sanityCheck();

        // first do trivial checks without using type hierarchy
        if (ntName.equals(primaryTypeName)) {
            return true;
        }
        Set mixins = ((NodeState) state).getMixinTypeNames();
        if (mixins.contains(ntName)) {
            return true;
        }

        // check effective node type
        return getEffectiveNodeType(mixins).includesNodeType(ntName);
    }

    /**
     * Returns the (internal) uuid of this node.
     *
     * @return the uuid of this node
     */
    public UUID internalGetUUID() {
        return ((NodeId) id).getUUID();
    }

    /**
     * Checks various pre-conditions that are common to all
     * <code>setProperty()</code> methods. The checks performed are:
     * <ul>
     * <li>this node must be checked-out</li>
     * <li>this node must not be locked by somebody else</li>
     * </ul>
     * Note that certain checks are performed by the respective
     * <code>Property.setValue()</code> methods.
     *
     * @throws VersionException    if this node is not checked-out
     * @throws LockException       if this node is locked by somebody else
     * @throws RepositoryException if another error occurs
     * @see javax.jcr.Node#setProperty
     */
    protected void checkSetProperty()
            throws VersionException, LockException, RepositoryException {
        // make sure this node is checked-out
        if (!internalIsCheckedOut()) {
            String msg = safeGetJCRPath()
                    + ": cannot set property of a checked-in node";
            log.debug(msg);
            throw new VersionException(msg);
        }

        // check lock status
        checkLock();
    }

    /**
     * Sets the internal value of a property without checking any constraints.
     * <p/>
     * Note that no type conversion is being performed, i.e. it's the caller's
     * responsibility to make sure that the type of the given value is compatible
     * with the specified property's definition.
     * @param name
     * @param value
     * @return
     * @throws ValueFormatException
     * @throws RepositoryException
     */
    protected Property internalSetProperty(Name name, InternalValue value)
            throws ValueFormatException, RepositoryException {
        int type;
        if (value == null) {
            type = PropertyType.UNDEFINED;
        } else {
            type = value.getType();
        }

        BitSet status = new BitSet();
        PropertyImpl prop = getOrCreateProperty(name, type, false, true, status);
        try {
            if (value == null) {
                prop.internalSetValue(null, type);
            } else {
                prop.internalSetValue(new InternalValue[]{value}, type);
            }
        } catch (RepositoryException re) {
            if (status.get(CREATED)) {
                // setting value failed, get rid of newly created property
                removeChildProperty(name);
            }
            // rethrow
            throw re;
        }
        return prop;
    }

    /**
     * Sets the internal value of a property without checking any constraints.
     * <p/>
     * Note that no type conversion is being performed, i.e. it's the caller's
     * responsibility to make sure that the type of the given values is compatible
     * with the specified property's definition.
     *
     * @param name
     * @param values
     * @return
     * @throws ValueFormatException
     * @throws RepositoryException
     */
    protected Property internalSetProperty(Name name, InternalValue[] values)
            throws ValueFormatException, RepositoryException {
        int type;
        if (values == null || values.length == 0
                || values[0] == null) {
            type = PropertyType.UNDEFINED;
        } else {
            type = values[0].getType();
        }
        return internalSetProperty(name, values, type);
    }

    /**
     * Sets the internal value of a property without checking any constraints.
     * <p/>
     * Note that no type conversion is being performed, i.e. it's the caller's
     * responsibility to make sure that the type of the given values is compatible
     * with the specified property's definition.
     *
     * @param name
     * @param values
     * @param type
     * @return
     * @throws ValueFormatException
     * @throws RepositoryException
     */
    protected Property internalSetProperty(Name name, InternalValue[] values,
                                           int type)
            throws ValueFormatException, RepositoryException {
        BitSet status = new BitSet();
        PropertyImpl prop = getOrCreateProperty(name, type, true, true, status);
        try {
            prop.internalSetValue(values, type);
        } catch (RepositoryException re) {
            if (status.get(CREATED)) {
                // setting value failed, get rid of newly created property
                removeChildProperty(name);
            }
            // rethrow
            throw re;
        }
        return prop;
    }

    /**
     * Returns the child node of <code>this</code> node with the specified
     * <code>name</code>.
     *
     * @param name The qualified name of the child node to retrieve.
     * @return The child node with the specified <code>name</code>.
     * @throws ItemNotFoundException If no child node exists with the
     *                               specified name.
     * @throws RepositoryException   If another error occurs.
     */
    public NodeImpl getNode(Name name) throws ItemNotFoundException, RepositoryException {
        return getNode(name, 1);
    }

    /**
     * Returns the child node of <code>this</code> node with the specified
     * <code>name</code>.
     *
     * @param name  The qualified name of the child node to retrieve.
     * @param index The index of the child node to retrieve (in the case of same-name siblings).
     * @return The child node with the specified <code>name</code>.
     * @throws ItemNotFoundException If no child node exists with the
     *                               specified name.
     * @throws RepositoryException   If another error occurs.
     */
    public NodeImpl getNode(Name name, int index)
            throws ItemNotFoundException, RepositoryException {
        // check state of this instance
        sanityCheck();

        NodeState thisState = (NodeState) state;
        if (index == 0) {
            index = 1;
        }
        NodeState.ChildNodeEntry cne = thisState.getChildNodeEntry(name, index);
        if (cne == null) {
            throw new ItemNotFoundException();
        }
        try {
            return (NodeImpl) itemMgr.getItem(cne.getId());
        } catch (AccessDeniedException ade) {
            throw new ItemNotFoundException();
        }
    }

    /**
     * Indicates whether a child node with the specified <code>name</code> exists.
     * Returns <code>true</code> if the child node exists and <code>false</code>
     * otherwise.
     *
     * @param name The qualified name of the child node.
     * @return <code>true</code> if the child node exists; <code>false</code> otherwise.
     * @throws RepositoryException If an unspecified error occurs.
     */
    public boolean hasNode(Name name) throws RepositoryException {
        return hasNode(name, 1);
    }

    /**
     * Indicates whether a child node with the specified <code>name</code> exists.
     * Returns <code>true</code> if the child node exists and <code>false</code>
     * otherwise.
     *
     * @param name  The qualified name of the child node.
     * @param index The index of the child node (in the case of same-name siblings).
     * @return <code>true</code> if the child node exists; <code>false</code> otherwise.
     * @throws RepositoryException If an unspecified error occurs.
     */
    public boolean hasNode(Name name, int index) throws RepositoryException {
        // check state of this instance
        sanityCheck();

        NodeState thisState = (NodeState) state;
        if (index == 0) {
            index = 1;
        }
        NodeState.ChildNodeEntry cne = thisState.getChildNodeEntry(name, index);
        if (cne == null) {
            return false;
        }
        return itemMgr.itemExists(cne.getId());
    }

    /**
     * Returns the property of <code>this</code> node with the specified
     * <code>name</code>.
     *
     * @param name The qualified name of the property to retrieve.
     * @return The property with the specified <code>name</code>.
     * @throws ItemNotFoundException If no property exists with the
     *                               specified name.
     * @throws RepositoryException   If another error occurs.
     */
    public PropertyImpl getProperty(Name name)
            throws ItemNotFoundException, RepositoryException {
        // check state of this instance
        sanityCheck();

        PropertyId propId = new PropertyId(getNodeId(), name);
        try {
            return (PropertyImpl) itemMgr.getItem(propId);
        } catch (AccessDeniedException ade) {
            throw new ItemNotFoundException(name.toString());
        }
    }

    /**
     * Indicates whether a property with the specified <code>name</code> exists.
     * Returns <code>true</code> if the property exists and <code>false</code>
     * otherwise.
     *
     * @param name The qualified name of the property.
     * @return <code>true</code> if the property exists; <code>false</code> otherwise.
     * @throws RepositoryException If an unspecified error occurs.
     */
    public boolean hasProperty(Name name) throws RepositoryException {
        // check state of this instance
        sanityCheck();

        NodeState thisState = (NodeState) state;
        if (!thisState.hasPropertyName(name)) {
            return false;
        }
        PropertyId propId = new PropertyId(thisState.getNodeId(), name);

        return itemMgr.itemExists(propId);
    }

    /**
     * Same as <code>{@link Node#addNode(String, String)}</code> except that
     * this method takes <code>Name</code> arguments instead of
     * <code>String</code>s and has an additional <code>uuid</code> argument.
     * <p/>
     * <b>Important Notice:</b> This method is for internal use only! Passing
     * already assigned uuid's might lead to unexpected results and
     * data corruption in the worst case.
     *
     * @param nodeName     name of the new node
     * @param nodeTypeName name of the new node's node type or <code>null</code>
     *                     if it should be determined automatically
     * @param uuid         uuid of the new node or <code>null</code> if a new
     *                     uuid should be assigned
     * @return the newly added node
     * @throws ItemExistsException
     * @throws NoSuchNodeTypeException
     * @throws VersionException
     * @throws ConstraintViolationException
     * @throws LockException
     * @throws RepositoryException
     */
    public synchronized NodeImpl addNode(Name nodeName, Name nodeTypeName,
                                         UUID uuid)
            throws ItemExistsException, NoSuchNodeTypeException, VersionException,
            ConstraintViolationException, LockException, RepositoryException {
        // check state of this instance
        sanityCheck();

        // make sure this node is checked-out
        if (!internalIsCheckedOut()) {
            String msg = safeGetJCRPath() + ": cannot add node to a checked-in node";
            log.debug(msg);
            throw new VersionException(msg);
        }

        // check lock status
        checkLock();

        NodeTypeImpl nt = null;
        if (nodeTypeName != null) {
            nt = session.getNodeTypeManager().getNodeType(nodeTypeName);
        }
        return internalAddChildNode(nodeName, nt, uuid == null ? null : new NodeId(uuid));
    }

    /**
     * Same as <code>{@link Node#setProperty(String, Value[])}</code> except that
     * this method takes a <code>Name</code> name argument instead of a
     * <code>String</code>.
     *
     * @param name
     * @param values
     * @return
     * @throws ValueFormatException
     * @throws VersionException
     * @throws LockException
     * @throws ConstraintViolationException
     * @throws RepositoryException
     */
    public PropertyImpl setProperty(Name name, Value[] values)
            throws ValueFormatException, VersionException, LockException,
            ConstraintViolationException, RepositoryException {

        // check state of this instance
        sanityCheck();

        // check pre-conditions for setting property
        checkSetProperty();

        int type;
        if (values == null || values.length == 0
                || values[0] == null) {
            type = PropertyType.UNDEFINED;
        } else {
            type = values[0].getType();
        }

        BitSet status = new BitSet();
        PropertyImpl prop = getOrCreateProperty(name, type, true, false, status);
        try {
            prop.setValue(values);
        } catch (RepositoryException re) {
            if (status.get(CREATED)) {
                // setting value failed, get rid of newly created property
                removeChildProperty(name);
            }
            // rethrow
            throw re;
        }
        return prop;
    }

    /**
     * Same as <code>{@link Node#setProperty(String, Value[], int)}</code> except
     * that this method takes a <code>Name</code> name argument instead of a
     * <code>String</code>.
     *
     * @param name
     * @param values
     * @param type
     * @return
     * @throws ValueFormatException
     * @throws VersionException
     * @throws LockException
     * @throws ConstraintViolationException
     * @throws RepositoryException
     */
    public PropertyImpl setProperty(Name name, Value[] values, int type)
            throws ValueFormatException, VersionException, LockException,
            ConstraintViolationException, RepositoryException {
        // check state of this instance
        sanityCheck();

        // check pre-conditions for setting property
        checkSetProperty();

        BitSet status = new BitSet();
        PropertyImpl prop = getOrCreateProperty(name, type, true, true, status);
        try {
            if (prop.getDefinition().getRequiredType() == PropertyType.UNDEFINED
                    && type != PropertyType.UNDEFINED) {
                prop.setValue(ValueHelper.convert(values, type, session.getValueFactory()));
            } else {
                prop.setValue(values);
            }
        } catch (RepositoryException re) {
            if (status.get(CREATED)) {
                // setting value failed, get rid of newly created property
                removeChildProperty(name);
            }
            // rethrow
            throw re;
        }
        return prop;
    }

    /**
     * Same as <code>{@link Node#setProperty(String, Value)}</code> except that
     * this method takes a <code>Name</code> name argument instead of a
     * <code>String</code>.
     *
     * @param name
     * @param value
     * @return
     * @throws ValueFormatException
     * @throws VersionException
     * @throws LockException
     * @throws ConstraintViolationException
     * @throws RepositoryException
     */
    public PropertyImpl setProperty(Name name, Value value)
            throws ValueFormatException, VersionException, LockException,
            ConstraintViolationException, RepositoryException {
        // check state of this instance
        sanityCheck();

        // check pre-conditions for setting property
        checkSetProperty();

        int type = PropertyType.UNDEFINED;
        if (value != null) {
            type = value.getType();
        }

        BitSet status = new BitSet();
        PropertyImpl prop = getOrCreateProperty(name, type, false, false, status);
        try {
            prop.setValue(value);
        } catch (RepositoryException re) {
            if (status.get(CREATED)) {
                // setting value failed, get rid of newly created property
                removeChildProperty(name);
            }
            // rethrow
            throw re;
        }
        return prop;
    }

    /**
     * @see ItemImpl#getQName()
     */
    public Name getQName() throws RepositoryException {
        return session.getHierarchyManager().getName(id);
    }

    /**
     * Returns the identifier of this <code>Node</code>.
     *
     * @return the id of this <code>Node</code>
     */
    public NodeId getNodeId() {
        return (NodeId) id;
    }

    /**
     * Same as <code>{@link Node#orderBefore(String, String)}</code> except that
     * this method takes a <code>Path.Element</code> arguments instead of
     * <code>String</code>s.
     *
     * @param srcName
     * @param dstName
     * @throws UnsupportedRepositoryOperationException
     * @throws VersionException
     * @throws ConstraintViolationException
     * @throws ItemNotFoundException
     * @throws LockException
     * @throws RepositoryException
     */
    public synchronized void orderBefore(Path.Element srcName,
                                         Path.Element dstName)
            throws UnsupportedRepositoryOperationException, VersionException,
            ConstraintViolationException, ItemNotFoundException, LockException,
            RepositoryException {

        // check state of this instance
        sanityCheck();

        if (!getPrimaryNodeType().hasOrderableChildNodes()) {
            throw new UnsupportedRepositoryOperationException(
                    "child node ordering not supported on node "
                    + safeGetJCRPath());
        }

        // check arguments
        if (srcName.equals(dstName)) {
            // there's nothing to do
            return;
        }

        // check existence
        if (!hasNode(srcName.getName(), srcName.getIndex())) {
            String name;
            try {
                Path.Element[] path = new Path.Element[] { srcName };
                name = session.getJCRPath(new PathBuilder(path).getPath());
            } catch (NameException e) {
                name = srcName.toString();
            } catch (NamespaceException e) {
                name = srcName.toString();
            }
            throw new ItemNotFoundException(safeGetJCRPath()
                    + " has no child node with name " + name);
        }
        if (dstName != null && !hasNode(dstName.getName(), dstName.getIndex())) {
            String name;
            try {
                Path.Element[] path = new Path.Element[] { dstName };
                name = session.getJCRPath(new PathBuilder(path).getPath());
            } catch (NameException e) {
                name = dstName.toString();
            } catch (NamespaceException e) {
                name = dstName.toString();
            }
            throw new ItemNotFoundException(safeGetJCRPath()
                    + " has no child node with name " + name);
        }

        // make sure this node is checked-out
        if (!internalIsCheckedOut()) {
            String msg = safeGetJCRPath()
                    + ": cannot change child node ordering of a checked-in node";
            log.debug(msg);
            throw new VersionException(msg);
        }

        // check protected flag
        if (definition.isProtected()) {
            String msg = safeGetJCRPath()
                    + ": cannot change child node ordering of a protected node";
            log.debug(msg);
            throw new ConstraintViolationException(msg);
        }

        // check lock status
        checkLock();

        ArrayList list = new ArrayList(((NodeState) state).getChildNodeEntries());
        int srcInd = -1, destInd = -1;
        for (int i = 0; i < list.size(); i++) {
            NodeState.ChildNodeEntry entry = (NodeState.ChildNodeEntry) list.get(i);
            if (srcInd == -1) {
                if (entry.getName().equals(srcName.getName())
                        && (entry.getIndex() == srcName.getIndex()
                        || srcName.getIndex() == 0 && entry.getIndex() == 1)) {
                    srcInd = i;
                }
            }
            if (destInd == -1 && dstName != null) {
                if (entry.getName().equals(dstName.getName())
                        && (entry.getIndex() == dstName.getIndex()
                        || dstName.getIndex() == 0 && entry.getIndex() == 1)) {
                    destInd = i;
                    if (srcInd != -1) {
                        break;
                    }
                }
            } else {
                if (srcInd != -1) {
                    break;
                }
            }
        }

        // check if resulting order would be different to current order
        if (destInd == -1) {
            if (srcInd == list.size() - 1) {
                // no change, we're done
                return;
            }
        } else {
            if ((destInd - srcInd) == 1) {
                // no change, we're done
                return;
            }
        }

        // reorder list
        if (destInd == -1) {
            list.add(list.remove(srcInd));
        } else {
            if (srcInd < destInd) {
                list.add(destInd, list.get(srcInd));
                list.remove(srcInd);
            } else {
                list.add(destInd, list.remove(srcInd));
            }
        }

        // modify the state of 'this', i.e. the parent node
        NodeState thisState = (NodeState) getOrCreateTransientItemState();
        thisState.setChildNodeEntries(list);
    }

    /**
     * Replaces the child node with the specified <code>id</code>
     * by a new child node with the same id and specified <code>nodeName</code>,
     * <code>nodeTypeName</code> and <code>mixinNames</code>.
     *
     * @param id           id of the child node to be replaced
     * @param nodeName     name of the new node
     * @param nodeTypeName name of the new node's node type
     * @param mixinNames   name of the new node's mixin types
     *
     * @return the new child node replacing the existing child
     * @throws ItemNotFoundException
     * @throws NoSuchNodeTypeException
     * @throws VersionException
     * @throws ConstraintViolationException
     * @throws LockException
     * @throws RepositoryException
     */
    public synchronized NodeImpl replaceChildNode(NodeId id, Name nodeName,
                                                  Name nodeTypeName,
                                                  Name[] mixinNames)
            throws ItemNotFoundException, NoSuchNodeTypeException, VersionException,
            ConstraintViolationException, LockException, RepositoryException {
        // check state of this instance
        sanityCheck();

        Node existing = (Node) itemMgr.getItem(id);

        // 'replace' is actually a 'remove existing/add new' operation;
        // this unfortunately changes the order of this node's
        // child node entries (JCR-1055);
        // => backup list of child node entries beforehand in order
        // to restore it afterwards
        NodeState.ChildNodeEntry cneExisting = ((NodeState) state).getChildNodeEntry(id);
        if (cneExisting == null) {
            throw new ItemNotFoundException(safeGetJCRPath()
                    + ": no child node entry with id " + id);
        }
        List cneList = new ArrayList(((NodeState) state).getChildNodeEntries());

        // remove existing
        existing.remove();

        // create new child node
        NodeImpl node = addNode(nodeName, nodeTypeName, id.getUUID());
        if (mixinNames != null) {
            for (int i = 0; i < mixinNames.length; i++) {
                node.addMixin(mixinNames[i]);
            }
        }

        // restore list of child node entries (JCR-1055)
        if (cneExisting.getName().equals(nodeName)) {
            // restore original child node list
            ((NodeState) state).setChildNodeEntries(cneList);
        } else {
            // replace child node entry with different name
            // but preserving original position
            ((NodeState) state).removeAllChildNodeEntries();
            for (Iterator iter = cneList.iterator(); iter.hasNext();) {
                NodeState.ChildNodeEntry cne = (NodeState.ChildNodeEntry) iter.next();
                if (cne.getId().equals(id)) {
                    // replace entry with different name
                    ((NodeState) state).addChildNodeEntry(nodeName, id);
                } else {
                    ((NodeState) state).addChildNodeEntry(cne.getName(), cne.getId());
                }
            }
        }

        return node;
    }

    //-----------------------------------------------------------------< Item >
    /**
     * {@inheritDoc}
     */
    public boolean isNode() {
        return true;
    }

    /**
     * {@inheritDoc}
     */
    public String getName() throws RepositoryException {
        // check state of this instance
        sanityCheck();

        if (state.getParentId() == null) {
            // this is the root node
            return "";
        }

        return session.getJCRName(session.getHierarchyManager().getName(id));
    }

    /**
     * {@inheritDoc}
     */
    public void accept(ItemVisitor visitor) throws RepositoryException {
        // check state of this instance
        sanityCheck();

        visitor.visit(this);
    }

    /**
     * {@inheritDoc}
     */
    public Node getParent()
            throws ItemNotFoundException, AccessDeniedException, RepositoryException {
        // check state of this instance
        sanityCheck();

        // check if root node
        NodeId parentId = state.getParentId();
        if (parentId == null) {
            String msg = "root node doesn't have a parent";
            log.debug(msg);
            throw new ItemNotFoundException(msg);
        }

        return (Node) itemMgr.getItem(parentId);
    }

    //-----------------------------------------------------------------< Node >
    /**
     * {@inheritDoc}
     */
    public synchronized Node addNode(String relPath)
            throws ItemExistsException, PathNotFoundException, VersionException,
            ConstraintViolationException, LockException, RepositoryException {
        // check state of this instance
        sanityCheck();

        return internalAddNode(relPath, null);
    }

    /**
     * {@inheritDoc}
     */
    public synchronized Node addNode(String relPath, String nodeTypeName)
            throws ItemExistsException, PathNotFoundException,
            NoSuchNodeTypeException, VersionException,
            ConstraintViolationException, LockException, RepositoryException {
        // check state of this instance
        sanityCheck();

        NodeTypeImpl nt = (NodeTypeImpl) session.getNodeTypeManager().getNodeType(nodeTypeName);
        return internalAddNode(relPath, nt);
    }

    /**
     * {@inheritDoc}
     */
    public void orderBefore(String srcName, String destName)
            throws UnsupportedRepositoryOperationException, VersionException,
            ConstraintViolationException, ItemNotFoundException, LockException,
            RepositoryException {

        Path.Element insertName;
        try {
            Path p = session.getQPath(srcName);
            // p must be a relative path of length==depth==1 (to eliminate e.g. "..")
            if (p.isAbsolute() || p.getLength() != 1 || p.getDepth() != 1) {
                throw new RepositoryException("invalid name: " + srcName);
            }
            insertName = p.getNameElement();
        } catch (NameException e) {
            String msg = "invalid name: " + srcName;
            log.debug(msg);
            throw new RepositoryException(msg, e);
        }

        Path.Element beforeName;
        if (destName != null) {
            try {
                Path p = session.getQPath(destName);
                // p must be a relative path of length==depth==1 (to eliminate e.g. "..")
                if (p.isAbsolute() || p.getLength() != 1 || p.getDepth() != 1) {
                    throw new RepositoryException("invalid name: " + destName);
                }
                beforeName = p.getNameElement();
            } catch (NameException e) {
                String msg = "invalid name: " + destName;
                log.debug(msg);
                throw new RepositoryException(msg, e);
            }
        } else {
            beforeName = null;
        }

        orderBefore(insertName, beforeName);
    }

    /**
     * {@inheritDoc}
     */
    public Property setProperty(String name, Value[] values)
            throws ValueFormatException, VersionException, LockException,
            ConstraintViolationException, RepositoryException {
        // check state of this instance
        sanityCheck();

        // check pre-conditions for setting property
        checkSetProperty();

        int type;
        if (values == null || values.length == 0
                || values[0] == null) {
            type = PropertyType.UNDEFINED;
        } else {
            type = values[0].getType();
        }

        BitSet status = new BitSet();
        PropertyImpl prop = getOrCreateProperty(name, type, true, false, status);
        try {
            if (prop.getDefinition().getRequiredType() == PropertyType.UNDEFINED
                    && type != PropertyType.UNDEFINED) {
                prop.setValue(ValueHelper.convert(values, type, session.getValueFactory()));
            } else {
                prop.setValue(values);
            }
        } catch (RepositoryException re) {
            if (status.get(CREATED)) {
                // setting value failed, get rid of newly created property
                removeChildProperty(name);
            }
            // rethrow
            throw re;
        }
        return prop;
    }

    /**
     * {@inheritDoc}
     */
    public Property setProperty(String name, Value[] values, int type)
            throws ValueFormatException, VersionException, LockException,
            ConstraintViolationException, RepositoryException {
        // check state of this instance
        sanityCheck();

        // check pre-conditions for setting property
        checkSetProperty();

        BitSet status = new BitSet();
        PropertyImpl prop = getOrCreateProperty(name, type, true, true, status);
        try {
            prop.setValue(values, type);
        } catch (RepositoryException re) {
            if (status.get(CREATED)) {
                // setting value failed, get rid of newly created property
                removeChildProperty(name);
            }
            // rethrow
            throw re;
        }
        return prop;
    }

    /**
     * {@inheritDoc}
     */
    public Property setProperty(String name, String[] values)
            throws ValueFormatException, VersionException, LockException,
            ConstraintViolationException, RepositoryException {
        /**
         * if the target property is not of type STRING then a
         * best-effort conversion is attempted
         */
        // check state of this instance
        sanityCheck();

        // check pre-conditions for setting property
        checkSetProperty();

        BitSet status = new BitSet();
        PropertyImpl prop = getOrCreateProperty(
                name, PropertyType.STRING, true, false, status);
        try {
            prop.setValue(values);
        } catch (RepositoryException re) {
            if (status.get(CREATED)) {
                // setting value failed, get rid of newly created property
                removeChildProperty(name);
            }
            // rethrow
            throw re;
        }
        return prop;
    }

    /**
     * {@inheritDoc}
     */
    public Property setProperty(String name, String[] values, int type)
            throws ValueFormatException, VersionException, LockException,
            ConstraintViolationException, RepositoryException {
        // check state of this instance
        sanityCheck();

        // check pre-conditions for setting property
        checkSetProperty();

        BitSet status = new BitSet();
        PropertyImpl prop = getOrCreateProperty(name, type, true, true, status);
        try {
            if (prop.getDefinition().getRequiredType() == PropertyType.UNDEFINED
                    && type != PropertyType.UNDEFINED) {
                prop.setValue(ValueHelper.convert(values, type, session.getValueFactory()));
            } else {
                prop.setValue(values);
            }
        } catch (RepositoryException re) {
            if (status.get(CREATED)) {
                // setting value failed, get rid of newly created property
                removeChildProperty(name);
            }
            // rethrow
            throw re;
        }
        return prop;
    }

    /**
     * {@inheritDoc}
     */
    public Property setProperty(String name, String value)
            throws ValueFormatException, VersionException, LockException,
            ConstraintViolationException, RepositoryException {
        // check state of this instance
        sanityCheck();

        // check pre-conditions for setting property
        checkSetProperty();

        BitSet status = new BitSet();
        PropertyImpl prop = getOrCreateProperty(
                name, PropertyType.STRING, false, false, status);
        try {
            prop.setValue(value);
        } catch (RepositoryException re) {
            if (status.get(CREATED)) {
                // setting value failed, get rid of newly created property
                removeChildProperty(name);
            }
            // rethrow
            throw re;
        }
        return prop;
    }

    /**
     * {@inheritDoc}
     */
    public Property setProperty(String name, String value, int type)
            throws ValueFormatException, VersionException, LockException,
            ConstraintViolationException, RepositoryException {
        // check state of this instance
        sanityCheck();

        // check pre-conditions for setting property
        checkSetProperty();

        BitSet status = new BitSet();
        PropertyImpl prop = getOrCreateProperty(name, type, false, true, status);
        try {
            if (prop.getDefinition().getRequiredType() == PropertyType.UNDEFINED
                    && type != PropertyType.UNDEFINED) {
                prop.setValue(ValueHelper.convert(value, type, session.getValueFactory()));
            } else {
                prop.setValue(value);
            }
        } catch (RepositoryException re) {
            if (status.get(CREATED)) {
                // setting value failed, get rid of newly created property
                removeChildProperty(name);
            }
            // rethrow
            throw re;
        }
        return prop;
    }

    /**
     * {@inheritDoc}
     */
    public Property setProperty(String name, Value value, int type)
            throws ValueFormatException, VersionException, LockException,
            ConstraintViolationException, RepositoryException {
        // check state of this instance
        sanityCheck();

        // check pre-conditions for setting property
        checkSetProperty();

        BitSet status = new BitSet();
        PropertyImpl prop = getOrCreateProperty(name, type, false, true, status);
        try {
            if (prop.getDefinition().getRequiredType() == PropertyType.UNDEFINED
                    && type != PropertyType.UNDEFINED) {
                prop.setValue(ValueHelper.convert(value, type, session.getValueFactory()));
            } else {
                prop.setValue(value);
            }
        } catch (RepositoryException re) {
            if (status.get(CREATED)) {
                // setting value failed, get rid of newly created property
                removeChildProperty(name);
            }
            // rethrow
            throw re;
        }
        return prop;
    }

    /**
     * {@inheritDoc}
     */
    public Property setProperty(String name, Value value)
            throws ValueFormatException, VersionException, LockException,
            ConstraintViolationException, RepositoryException {
        // check state of this instance
        sanityCheck();

        // check pre-conditions for setting property
        checkSetProperty();

        int type = PropertyType.UNDEFINED;
        if (value != null) {
            type = value.getType();
        }

        BitSet status = new BitSet();
        PropertyImpl prop = getOrCreateProperty(name, type, false, false, status);
        try {
            if (prop.getDefinition().getRequiredType() == PropertyType.UNDEFINED
                    && type != PropertyType.UNDEFINED) {
                prop.setValue(ValueHelper.convert(value, type, session.getValueFactory()));
            } else {
                prop.setValue(value);
            }
        } catch (RepositoryException re) {
            if (status.get(CREATED)) {
                // setting value failed, get rid of newly created property
                removeChildProperty(name);
            }
            // rethrow
            throw re;
        }
        return prop;
    }

    /**
     * {@inheritDoc}
     */
    public Property setProperty(String name, InputStream value)
            throws ValueFormatException, VersionException, LockException,
            ConstraintViolationException, RepositoryException {
        // check state of this instance
        sanityCheck();

        // check pre-conditions for setting property
        checkSetProperty();

        BitSet status = new BitSet();
        PropertyImpl prop = getOrCreateProperty(
                name, PropertyType.BINARY, false, false, status);
        try {
            prop.setValue(value);
        } catch (RepositoryException re) {
            if (status.get(CREATED)) {
                // setting value failed, get rid of newly created property
                removeChildProperty(name);
            }
            // rethrow
            throw re;
        }
        return prop;
    }

    /**
     * {@inheritDoc}
     */
    public Property setProperty(String name, boolean value)
            throws ValueFormatException, VersionException, LockException,
            ConstraintViolationException, RepositoryException {
        // check state of this instance
        sanityCheck();

        // check pre-conditions for setting property
        checkSetProperty();

        BitSet status = new BitSet();
        PropertyImpl prop = getOrCreateProperty(
                name, PropertyType.BOOLEAN, false, false, status);
        try {
            prop.setValue(value);
        } catch (RepositoryException re) {
            if (status.get(CREATED)) {
                // setting value failed, get rid of newly created property
                removeChildProperty(name);
            }
            // rethrow
            throw re;
        }
        return prop;
    }

    /**
     * {@inheritDoc}
     */
    public Property setProperty(String name, double value)
            throws ValueFormatException, VersionException, LockException,
            ConstraintViolationException, RepositoryException {
        // check state of this instance
        sanityCheck();

        // check pre-conditions for setting property
        checkSetProperty();

        BitSet status = new BitSet();
        PropertyImpl prop = getOrCreateProperty(
                name, PropertyType.DOUBLE, false, false, status);
        try {
            prop.setValue(value);
        } catch (RepositoryException re) {
            if (status.get(CREATED)) {
                // setting value failed, get rid of newly created property
                removeChildProperty(name);
            }
            // rethrow
            throw re;
        }
        return prop;
    }

    /**
     * {@inheritDoc}
     */
    public Property setProperty(String name, long value)
            throws ValueFormatException, VersionException, LockException,
            ConstraintViolationException, RepositoryException {
        // check state of this instance
        sanityCheck();

        // check pre-conditions for setting property
        checkSetProperty();

        BitSet status = new BitSet();
        PropertyImpl prop = getOrCreateProperty(
                name, PropertyType.LONG, false, false, status);
        try {
            prop.setValue(value);
        } catch (RepositoryException re) {
            if (status.get(CREATED)) {
                // setting value failed, get rid of newly created property
                removeChildProperty(name);
            }
            // rethrow
            throw re;
        }
        return prop;
    }

    /**
     * {@inheritDoc}
     */
    public Property setProperty(String name, Calendar value)
            throws ValueFormatException, VersionException, LockException,
            ConstraintViolationException, RepositoryException {
        // check state of this instance
        sanityCheck();

        // check pre-conditions for setting property
        checkSetProperty();

        BitSet status = new BitSet();
        PropertyImpl prop = getOrCreateProperty(
                name, PropertyType.DATE, false, false, status);
        try {
            prop.setValue(value);
        } catch (RepositoryException re) {
            if (status.get(CREATED)) {
                // setting value failed, get rid of newly created property
                removeChildProperty(name);
            }
            // rethrow
            throw re;
        }
        return prop;
    }

    /**
     * {@inheritDoc}
     */
    public Property setProperty(String name, Node value)
            throws ValueFormatException, VersionException, LockException,
            ConstraintViolationException, RepositoryException {
        // check state of this instance
        sanityCheck();

        // check pre-conditions for setting property
        checkSetProperty();

        BitSet status = new BitSet();
        PropertyImpl prop = getOrCreateProperty(
                name, PropertyType.REFERENCE, false, true, status);
        try {
            prop.setValue(value);
        } catch (RepositoryException re) {
            if (status.get(CREATED)) {
                // setting value failed, get rid of newly created property
                removeChildProperty(name);
            }
            // rethrow
            throw re;
        }
        return prop;
    }

    /**
     * {@inheritDoc}
     */
    public Node getNode(String relPath)
            throws PathNotFoundException, RepositoryException {
        // check state of this instance
        sanityCheck();
        NodeId id = resolveRelativeNodePath(relPath);
        if (id == null) {
            throw new PathNotFoundException(relPath);
        }
        try {
            return (Node) itemMgr.getItem(id);
        } catch (AccessDeniedException ade) {
            throw new PathNotFoundException(relPath);
        } catch (ItemNotFoundException infe) {
            throw new PathNotFoundException(relPath);
        }
    }

    /**
     * {@inheritDoc}
     */
    public NodeIterator getNodes() throws RepositoryException {
        // check state of this instance
        sanityCheck();

        /**
         * IMPORTANT:
         * an implementation of Node.getNodes()
         * must not use a class derived from TraversingElementVisitor
         * to traverse the hierarchy because this would lead to an infinite
         * recursion!
         */
        try {
            return itemMgr.getChildNodes((NodeId) id);
        } catch (ItemNotFoundException infe) {
            String msg = "failed to list the child nodes of " + safeGetJCRPath();
            log.debug(msg);
            throw new RepositoryException(msg, infe);
        } catch (AccessDeniedException ade) {
            String msg = "failed to list the child nodes of " + safeGetJCRPath();
            log.debug(msg);
            throw new RepositoryException(msg, ade);
        }
    }

    /**
     * {@inheritDoc}
     */
    public PropertyIterator getProperties() throws RepositoryException {
        // check state of this instance
        sanityCheck();

        /**
         * IMPORTANT:
         * an implementation of Node.getProperties()
         * must not use a class derived from TraversingElementVisitor
         * to traverse the hierarchy because this would lead to an infinite
         * recursion!
         */
        try {
            return itemMgr.getChildProperties((NodeId) id);
        } catch (ItemNotFoundException infe) {
            String msg = "failed to list the child properties of " + safeGetJCRPath();
            log.debug(msg);
            throw new RepositoryException(msg, infe);
        } catch (AccessDeniedException ade) {
            String msg = "failed to list the child properties of " + safeGetJCRPath();
            log.debug(msg);
            throw new RepositoryException(msg, ade);
        }
    }

    /**
     * {@inheritDoc}
     */
    public Property getProperty(String relPath)
            throws PathNotFoundException, RepositoryException {
        // check state of this instance
        sanityCheck();

        PropertyId id = resolveRelativePropertyPath(relPath);
        if (id == null) {
            throw new PathNotFoundException(relPath);
        }
        try {
            return (Property) itemMgr.getItem(id);
        } catch (ItemNotFoundException infe) {
            throw new PathNotFoundException(relPath);
        } catch (AccessDeniedException ade) {
            throw new PathNotFoundException(relPath);
        }
    }

    /**
     * {@inheritDoc}
     */
    public boolean hasNode(String relPath) throws RepositoryException {
        // check state of this instance
        sanityCheck();

        NodeId id = resolveRelativeNodePath(relPath);
        if (id != null) {
            return itemMgr.itemExists(id);
        } else {
            return false;
        }
    }

    /**
     * {@inheritDoc}
     */
    public boolean hasNodes() throws RepositoryException {
        // check state of this instance
        sanityCheck();

        /**
         * hasNodes respects the access rights
         * of this node's session, i.e. it will
         * return false if child nodes exist
         * but the session is not granted read-access
         */
        return itemMgr.hasChildNodes((NodeId) id);
    }

    /**
     * {@inheritDoc}
     */
    public boolean hasProperties() throws RepositoryException {
        // check state of this instance
        sanityCheck();

        /**
         * hasProperties respects the access rights
         * of this node's session, i.e. it will
         * return false if properties exist
         * but the session is not granted read-access
         */
        return itemMgr.hasChildProperties((NodeId) id);
    }

    /**
     * {@inheritDoc}
     */
    public boolean isNodeType(String nodeTypeName) throws RepositoryException {
        try {
            return isNodeType(session.getQName(nodeTypeName));
        } catch (NameException e) {
            throw new RepositoryException(
                    "invalid node type name: " + nodeTypeName, e);
        }
    }

    /**
     * {@inheritDoc}
     */
    public NodeType getPrimaryNodeType() throws RepositoryException {
        // check state of this instance
        sanityCheck();

        return session.getNodeTypeManager().getNodeType(primaryTypeName);
    }

    /**
     * {@inheritDoc}
     */
    public NodeType[] getMixinNodeTypes() throws RepositoryException {
        // check state of this instance
        sanityCheck();

        Set mixinNames = ((NodeState) state).getMixinTypeNames();
        if (mixinNames.isEmpty()) {
            return new NodeType[0];
        }
        NodeType[] nta = new NodeType[mixinNames.size()];
        Iterator iter = mixinNames.iterator();
        int i = 0;
        while (iter.hasNext()) {
            nta[i++] = session.getNodeTypeManager().getNodeType((Name) iter.next());
        }
        return nta;
    }

    /**
     * {@inheritDoc}
     */
    public void addMixin(String mixinName)
            throws NoSuchNodeTypeException, VersionException,
            ConstraintViolationException, LockException, RepositoryException {
        try {
            addMixin(session.getQName(mixinName));
        } catch (NameException e) {
            throw new RepositoryException(
                    "invalid mixin type name: " + mixinName, e);
        }
    }

    /**
     * {@inheritDoc}
     */
    public void removeMixin(String mixinName)
            throws NoSuchNodeTypeException, VersionException,
            ConstraintViolationException, LockException, RepositoryException {
        try {
            removeMixin(session.getQName(mixinName));
        } catch (NameException e) {
            throw new RepositoryException(
                    "invalid mixin type name: " + mixinName, e);
        }
    }

    /**
     * {@inheritDoc}
     */
    public boolean canAddMixin(String mixinName)
            throws NoSuchNodeTypeException, RepositoryException {
        // check state of this instance
        sanityCheck();

        // check checked-out status
        if (!internalIsCheckedOut()) {
            return false;
        }

        // check protected flag
        if (definition.isProtected()) {
            return false;
        }

        // check lock status
        try {
            checkLock();
        } catch (LockException le) {
            return false;
        }

        Name ntName;
        try {
            ntName = session.getQName(mixinName);
        } catch (NameException e) {
            throw new RepositoryException(
                    "invalid mixin type name: " + mixinName, e);
        }

        NodeTypeManagerImpl ntMgr = session.getNodeTypeManager();
        NodeTypeImpl mixin = ntMgr.getNodeType(ntName);
        if (!mixin.isMixin()) {
            return false;
        }
        NodeTypeImpl primaryType = ntMgr.getNodeType(primaryTypeName);
        if (primaryType.isDerivedFrom(ntName)) {
            return false;
        }

        // build effective node type of mixins & primary type
        // in order to detect conflicts
        NodeTypeRegistry ntReg = ntMgr.getNodeTypeRegistry();
        EffectiveNodeType entExisting;
        try {
            // existing mixin's
            HashSet set = new HashSet(((NodeState) state).getMixinTypeNames());
            // primary type
            set.add(primaryTypeName);
            // build effective node type representing primary type including existing mixin's
            entExisting = ntReg.getEffectiveNodeType((Name[]) set.toArray(new Name[set.size()]));
            if (entExisting.includesNodeType(ntName)) {
                return false;
            }
            // add new mixin
            set.add(ntName);
            // try to build new effective node type (will throw in case of conflicts)
            ntReg.getEffectiveNodeType((Name[]) set.toArray(new Name[set.size()]));
        } catch (NodeTypeConflictException ntce) {
            return false;
        }

        return true;
    }

    /**
     * {@inheritDoc}
     */
    public boolean hasProperty(String relPath) throws RepositoryException {
        // check state of this instance
        sanityCheck();

        PropertyId id = resolveRelativePropertyPath(relPath);
        if (id != null) {
            return itemMgr.itemExists(id);
        } else {
            return false;
        }
    }

    /**
     * {@inheritDoc}
     */
    public PropertyIterator getReferences() throws RepositoryException {
        return getReferences(null);
    }

    /**
     * {@inheritDoc}
     */
    public NodeDefinition getDefinition() throws RepositoryException {
        // check state of this instance
        sanityCheck();

        return definition;
    }

    /**
     * {@inheritDoc}
     */
    public NodeIterator getNodes(String namePattern) throws RepositoryException {
        // check state of this instance
        sanityCheck();

        ArrayList nodes = new ArrayList();
        // traverse children using a special filtering 'collector'
        accept(new ChildrenCollectorFilter(namePattern, nodes, true, false, 1));
        return new NodeIteratorAdapter(nodes);
    }

    /**
     * {@inheritDoc}
     */
    public PropertyIterator getProperties(String namePattern)
            throws RepositoryException {
        // check state of this instance
        sanityCheck();

        ArrayList properties = new ArrayList();
        // traverse children using a special filtering 'collector'
        accept(new ChildrenCollectorFilter(namePattern, properties, false, true, 1));
        return new PropertyIteratorAdapter(properties);
    }

    /**
     * {@inheritDoc}
     */
    public Item getPrimaryItem()
            throws ItemNotFoundException, RepositoryException {
        // check state of this instance
        sanityCheck();

        String name = getPrimaryNodeType().getPrimaryItemName();
        if (name == null) {
            throw new ItemNotFoundException();
        }
        if (hasProperty(name)) {
            return getProperty(name);
        } else if (hasNode(name)) {
            return getNode(name);
        } else {
            throw new ItemNotFoundException();
        }
    }

    /**
     * {@inheritDoc}
     */
    public String getUUID()
            throws UnsupportedRepositoryOperationException, RepositoryException {
        // check state of this instance
        sanityCheck();

        if (!isNodeType(NameConstants.MIX_REFERENCEABLE)) {
            throw new UnsupportedRepositoryOperationException();
        }

        return internalGetUUID().toString();
    }

    /**
     * {@inheritDoc}
     */
    public String getCorrespondingNodePath(String workspaceName)
            throws ItemNotFoundException, NoSuchWorkspaceException,
            AccessDeniedException, RepositoryException {
        // check state of this instance
        sanityCheck();

        SessionImpl srcSession = null;
        try {
            // create session on other workspace for current subject
            // (may throw NoSuchWorkspaceException and AccessDeniedException)
            srcSession = rep.createSession(session.getSubject(), workspaceName);

            // search nearest ancestor that is referenceable
            NodeImpl m1 = this;
            while (m1.getDepth() != 0 && !m1.isNodeType(NameConstants.MIX_REFERENCEABLE)) {
                m1 = (NodeImpl) m1.getParent();
            }

            // if root is common ancestor, corresponding path is same as ours
            if (m1.getDepth() == 0) {
                // check existence
                if (!srcSession.getItemManager().itemExists(getPrimaryPath())) {
                    throw new ItemNotFoundException(safeGetJCRPath());
                } else {
                    return getPath();
                }
            }

            // get corresponding ancestor
            Node m2 = srcSession.getNodeByUUID(m1.getUUID());

            // return path of m2, if m1 == n1
            if (m1 == this) {
                return m2.getPath();
            }

            String relPath;
            try {
                Path p = m1.getPrimaryPath().computeRelativePath(getPrimaryPath());
                // use prefix mappings of srcSession
                relPath = session.getJCRPath(p);
            } catch (NameException be) {
                // should never get here...
                String msg = "internal error: failed to determine relative path";
                log.error(msg, be);
                throw new RepositoryException(msg, be);
            }

            if (!m2.hasNode(relPath)) {
                throw new ItemNotFoundException();
            } else {
                return m2.getNode(relPath).getPath();
            }
        } finally {
            if (srcSession != null) {
                // we don't need the other session anymore, logout
                srcSession.logout();
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    public int getIndex() throws RepositoryException {
        // check state of this instance
        sanityCheck();

        NodeId parentId = state.getParentId();
        if (parentId == null) {
            // the root node cannot have same-name siblings; always return 1
            return 1;
        }

        try {
            NodeState parent =
                    (NodeState) stateMgr.getItemState(parentId);
            NodeState.ChildNodeEntry parentEntry =
                    parent.getChildNodeEntry(getNodeId());
            return parentEntry.getIndex();
        } catch (ItemStateException ise) {
            // should never get here...
            String msg = "internal error: failed to determine index";
            log.error(msg, ise);
            throw new RepositoryException(msg, ise);
        }
    }

    //------------------------------< versioning support: public Node methods >
    /**
     * {@inheritDoc}
     */
    public Version checkin()
            throws VersionException, UnsupportedRepositoryOperationException,
            InvalidItemStateException, LockException, RepositoryException {
        // check state of this instance
        sanityCheck();

        // check if versionable
        checkVersionable();

        // check if checked out
        if (!internalIsCheckedOut()) {
            String msg = safeGetJCRPath() + ": Node is already checked-in. ignoring.";
            log.debug(msg);
            return getBaseVersion();
        }

        // check for pending changes
        if (hasPendingChanges()) {
            String msg = "Unable to checkin node. Node has pending changes: " + safeGetJCRPath();
            log.debug(msg);
            throw new InvalidItemStateException(msg);
        }

        // check lock status
        checkLock();

        Version v = session.getVersionManager().checkin(this);
        internalSetProperty(NameConstants.JCR_ISCHECKEDOUT, InternalValue.create(false));
        internalSetProperty(NameConstants.JCR_BASEVERSION, InternalValue.create(new UUID(v.getUUID())));
        internalSetProperty(NameConstants.JCR_PREDECESSORS, InternalValue.EMPTY_ARRAY, PropertyType.REFERENCE);
        save();
        return v;
    }

    /**
     * {@inheritDoc}
     */
    public void checkout()
            throws UnsupportedRepositoryOperationException, LockException,
            RepositoryException {
        // check state of this instance
        sanityCheck();

        // check if versionable
        checkVersionable();

        // check checked-out status
        if (internalIsCheckedOut()) {
            String msg = safeGetJCRPath() + ": Node is already checked-out. ignoring.";
            log.debug(msg);
            return;
        }

        // check lock status
        checkLock();

        boolean hasPendingChanges = session.hasPendingChanges();

        Property[] props = new Property[2];
        props[0] = internalSetProperty(NameConstants.JCR_ISCHECKEDOUT, InternalValue.create(true));
        props[1] = internalSetProperty(NameConstants.JCR_PREDECESSORS,
                new InternalValue[]{
                        InternalValue.create(new UUID(getBaseVersion().getUUID()))
                });
        if (hasPendingChanges) {
            for (int i = 0; i < props.length; i++) {
                props[i].save();
            }
        } else {
            try {
                session.save();
            } catch (RepositoryException e) {
                session.refresh(false);
                throw e;
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    public void update(String srcWorkspaceName)
            throws NoSuchWorkspaceException, AccessDeniedException,
            LockException, InvalidItemStateException, RepositoryException {
        internalMerge(srcWorkspaceName, null, false);
    }

    /**
     * {@inheritDoc}
     */
    public NodeIterator merge(String srcWorkspace, boolean bestEffort)
            throws NoSuchWorkspaceException, AccessDeniedException,
            VersionException, LockException, InvalidItemStateException,
            RepositoryException {

        List failedIds = new ArrayList();
        internalMerge(srcWorkspace, failedIds, bestEffort);

        return new LazyItemIterator(itemMgr, failedIds);
    }

    public void cancelMerge(Version version)
            throws VersionException, InvalidItemStateException,
            UnsupportedRepositoryOperationException, RepositoryException {
        internalFinishMerge(version, true);
    }

    /**
     * {@inheritDoc}
     */
    public void doneMerge(Version version) throws VersionException,
            InvalidItemStateException, UnsupportedRepositoryOperationException,
            RepositoryException {
        internalFinishMerge(version, false);
    }

    /**
     * {@inheritDoc}
     */
    public boolean isCheckedOut() throws RepositoryException {
        // check state of this instance
        sanityCheck();

        return internalIsCheckedOut();
    }

    /**
     * {@inheritDoc}
     */
    public void restore(String versionName, boolean removeExisting)
            throws VersionException, ItemExistsException,
            UnsupportedRepositoryOperationException, LockException,
            InvalidItemStateException, RepositoryException {

        // checks
        sanityCheck();
        checkSessionHasPending();
        checkLock();

        Version v = getVersionHistory().getVersion(versionName);
        DateVersionSelector gvs = new DateVersionSelector(v.getCreated());
        internalRestore(v, gvs, removeExisting);
        // session.save/revert is done in internal restore
    }

    /**
     * {@inheritDoc}
     */
    public void restore(Version version, boolean removeExisting)
            throws VersionException, ItemExistsException,
            UnsupportedRepositoryOperationException, LockException,
            RepositoryException {

        // do checks
        sanityCheck();
        checkSessionHasPending();
        checkVersionable();
        checkLock();

        // check if 'own' version
        if (!version.getContainingHistory().isSame(getVersionHistory())) {
            throw new VersionException("Unable to restore version. Not same version history.");
        }

        internalRestore(version, new DateVersionSelector(version.getCreated()), removeExisting);
        // session.save/revert is done in internal restore
    }

    /**
     * {@inheritDoc}
     */
    public void restore(Version version, String relPath, boolean removeExisting)
            throws PathNotFoundException, ItemExistsException, VersionException,
            ConstraintViolationException, UnsupportedRepositoryOperationException,
            LockException, InvalidItemStateException, RepositoryException {

        // do checks
        sanityCheck();
        checkSessionHasPending();
        checkLock();

        // if node exists, do a 'normal' restore
        if (hasNode(relPath)) {
            getNode(relPath).restore(version, removeExisting);
        } else {
            NodeImpl node;
            try {
                // check if versionable node exists
                InternalFrozenNode fn = ((VersionImpl) version).getFrozenNode();
                node = (NodeImpl) session.getNodeByUUID(fn.getFrozenUUID());
                if (removeExisting) {
                    try {
                        Path relative = session.getQPath(relPath);
                        Path dstPath =
                            PathFactoryImpl.getInstance().create(getPrimaryPath(), relative, false)
                            .getCanonicalPath();
                        // move to respective location
                        session.move(node.getPath(), session.getJCRPath(dstPath));
                        // need to refetch ?
                        node = (NodeImpl) session.getNodeByUUID(fn.getFrozenUUID());
                    } catch (NameException e) {
                        throw new RepositoryException(e);
                    }
                } else {
                    throw new ItemExistsException("Unable to restore version. Versionable node already exists.");
                }
            } catch (ItemNotFoundException e) {
                // not found, create new one
                node = addNode(relPath, ((VersionImpl) version).getFrozenNode());
            }

            // recreate node from frozen state
            node.internalRestore(version, new DateVersionSelector(version.getCreated()), removeExisting);
            // session.save/revert is done in internal restore
        }
    }

    /**
     * {@inheritDoc}
     */
    public void restoreByLabel(String versionLabel, boolean removeExisting)
            throws VersionException, ItemExistsException,
            UnsupportedRepositoryOperationException, LockException,
            InvalidItemStateException, RepositoryException {

        // do checks
        sanityCheck();
        checkSessionHasPending();
        checkLock();

        Version v = getVersionHistory().getVersionByLabel(versionLabel);
        if (v == null) {
            throw new VersionException("No version for label " + versionLabel + " found.");
        }
        internalRestore(v, new LabelVersionSelector(versionLabel), removeExisting);
        // session.save/revert is done in internal restore
    }

    /**
     * {@inheritDoc}
     */
    public VersionHistory getVersionHistory()
            throws UnsupportedRepositoryOperationException, RepositoryException {
        // check state of this instance
        sanityCheck();

        checkVersionable();

        // transactions workaround.
        NodeId id = NodeId.valueOf(getProperty(NameConstants.JCR_VERSIONHISTORY).getString());
        session.getVersionManager().getVersionHistory(id);

        return (VersionHistory) getProperty(NameConstants.JCR_VERSIONHISTORY).getNode();
    }

    /**
     * {@inheritDoc}
     */
    public Version getBaseVersion()
            throws UnsupportedRepositoryOperationException, RepositoryException {
        // check state of this instance
        sanityCheck();

        checkVersionable();

        // transactions workaround.
        NodeId id = NodeId.valueOf(getProperty(NameConstants.JCR_BASEVERSION).getString());
        session.getVersionManager().getVersion(id);

        return (Version) getProperty(NameConstants.JCR_BASEVERSION).getNode();
    }

    //-----------------------------------< versioning support: implementation >
    /**
     * Checks if this node is versionable, i.e. has 'mix:versionable'.
     *
     * @throws UnsupportedRepositoryOperationException
     *          if this node is not versionable
     */
    private void checkVersionable()
            throws UnsupportedRepositoryOperationException, RepositoryException {
        if (!isNodeType(NameConstants.MIX_VERSIONABLE)) {
            String msg = "Unable to perform versioning operation on non versionable node: " + safeGetJCRPath();
            log.debug(msg);
            throw new UnsupportedRepositoryOperationException(msg);
        }
    }

    /**
     * Checks if this nodes session has pending changes.
     *
     * @throws InvalidItemStateException if this nodes session has pending changes
     * @throws RepositoryException
     */
    private void checkSessionHasPending()
            throws InvalidItemStateException, RepositoryException {
        // check for pending changes
        if (session.hasPendingChanges()) {
            String msg = "Unable to perform operation. Session has pending changes.";
            log.debug(msg);
            throw new InvalidItemStateException(msg);
        }


    }

    /**
     * Returns the corresponding node in the workspace of the given session.
     * <p/>
     * Given a node N1 in workspace W1, its corresponding node N2 in workspace
     * W2 is defined as follows:
     * <ul>
     * <li>If N1 is the root node of W1 then N2 is the root node of W2.
     * <li>If N1 is referenceable (has a UUID) then N2 is the node in W2 with
     * the same UUID.
     * <li>If N1 is not referenceable (does not have a UUID) then there is some
     * node M1 which is either the nearest ancestor of N1 that is
     * referenceable, or is the root node of W1. If the corresponding node
     * of M1 is M2 in W2, then N2 is the node with the same relative path
     * from M2 as N1 has from M1.
     * </ul>
     *
     * @param srcSession
     * @return the corresponding node or <code>null</code> if no corresponding
     *         node exists.
     * @throws RepositoryException If another error occurs.
     */
    private NodeImpl getCorrespondingNode(SessionImpl srcSession)
            throws AccessDeniedException, RepositoryException {

        // search nearest ancestor that is referenceable
        NodeImpl m1 = this;
        while (!m1.isNodeType(NameConstants.MIX_REFERENCEABLE)) {
            if (m1.getDepth() == 0) {
                // root node
                try {
                    return (NodeImpl) srcSession.getItem(getPath());
                } catch (PathNotFoundException e) {
                    return null;
                }
            }
            m1 = (NodeImpl) m1.getParent();
        }

        try {
            // get corresponding ancestor (might throw ItemNotFoundException)
            NodeImpl m2 = (NodeImpl) srcSession.getNodeByUUID(m1.getUUID());

            // return m2, if m1 == n1
            if (m1 == this) {
                return m2;
            }

            String relPath;
            try {
                Path p = m1.getPrimaryPath().computeRelativePath(getPrimaryPath());
                // use prefix mappings of srcSession
                relPath = session.getJCRPath(p);
            } catch (NameException be) {
                // should never get here...
                String msg = "internal error: failed to determine relative path";
                log.error(msg, be);
                throw new RepositoryException(msg, be);
            }
            if (!m2.hasNode(relPath)) {
                return null;
            } else {
                return (NodeImpl) m2.getNode(relPath);
            }
        } catch (ItemNotFoundException e) {
            return null;
        }
    }

    /**
     * Performs the merge test. If the result is 'update', then the corresponding
     * source node is returned. if the result is 'leave' or 'besteffort-fail'
     * then <code>null</code> is returned. If the result of the merge test is
     * 'fail' with bestEffort set to <code>false</code> a MergeException is
     * thrown.
     * <p/>
     * jsr170 - 8.2.10 Merge:
     * [...]
     * <ul>
     * <li>If N is currently checked-in then:</li>
     * <ul>
     * <li>If V' is a successor (to any degree) of V, then the merge result
     *     for N is update.
     * </li>
     * <li>If V' is a predecessor (to any degree) of V or if V and
     *     V' are identical (i.e., are actually the same version),
     *     then the merge result for N is leave.
     * </li>
     * <li>If V is neither a successor of, predecessor of, nor
     *     identical with V', then the merge result for N is failed.
     *     This is the case where N and N' represent divergent
     *     branches of the version graph, thus determining the
     *     result of a merge is non-trivial.
     * </li>
     * </ul>
     * <li>If N is currently checked-out then:</li>
     * <ul>
     * <li>If V' is a predecessor (to any degree) of V or if V and
     *     V' are identical (i.e., are actually the same version),
     *     then the merge result for N is leave.
     * </li>
     * <li>If any other relationship holds between V and V',
     *     then the merge result for N is fail.
     * </li>
     * </ul>
     * </ul>
     *
     * @param srcSession the source session
     * @param failedIds the list to store the failed node ids.
     * @param bestEffort the best effort flag
     * @return the corresponding source node or <code>null</code>
     * @throws RepositoryException if an error occurs.
     * @throws AccessDeniedException if access is denied
     */
    private NodeImpl doMergeTest(SessionImpl srcSession, List failedIds, boolean bestEffort)
            throws RepositoryException, AccessDeniedException {

        // If N does not have a corresponding node then the merge result for N is leave.
        NodeImpl srcNode = getCorrespondingNode(srcSession);
        if (srcNode == null) {
            return null;
        }

        // if not versionable, update
        if (!isNodeType(NameConstants.MIX_VERSIONABLE) || failedIds == null) {
            return srcNode;
        }
        // if source node is not versionable, leave
        if (!srcNode.isNodeType(NameConstants.MIX_VERSIONABLE)) {
            return null;
        }
        // test versions
        VersionImpl v = (VersionImpl) getBaseVersion();
        VersionImpl vp = (VersionImpl) srcNode.getBaseVersion();
        if (vp.isMoreRecent(v) && !isCheckedOut()) {
            // I f V' is a successor (to any degree) of V, then the merge result for
            // N is update. This case can be thought of as the case where N' is
            // "newer" than N and therefore N should be updated to reflect N'.
            return srcNode;
        } else if (v.isSame(vp) || v.isMoreRecent(vp)) {
            // If V' is a predecessor (to any degree) of V or if V and V' are
            // identical (i.e., are actually the same version), then the merge
            // result for N is leave. This case can be thought of as the case where
            // N' is "older" or the "same age" as N and therefore N should be left alone.
            return null;
        } else {
            // If V is neither a successor of, predecessor of, nor identical
            // with V', then the merge result for N is failed. This is the case
            // where N and N' represent divergent branches of the version graph,
            // thus determining the result of a merge is non-trivial.
            if (bestEffort) {
                // add 'offending' version to jcr:mergeFailed property
                Set set = internalGetMergeFailed();
                set.add(srcNode.getBaseVersion().getUUID());
                internalSetMergeFailed(set);
                failedIds.add(state.getId());
                return null;
            } else {
                String msg = "Unable to merge nodes. Violating versions. " + safeGetJCRPath();
                log.debug(msg);
                throw new MergeException(msg);
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    private void internalFinishMerge(Version version, boolean cancel)
            throws VersionException, InvalidItemStateException,
            UnsupportedRepositoryOperationException, RepositoryException {
        // check state of this instance
        sanityCheck();

        // check for pending changes
        if (hasPendingChanges()) {
            String msg = "Unable to finish merge. Node has pending changes: " + safeGetJCRPath();
            log.debug(msg);
            throw new InvalidItemStateException(msg);
        }

        // check versionable
        checkVersionable();

        // check lock
        checkLock();

        // check if checked out
        if (!internalIsCheckedOut()) {
            String msg = "Unable to finish merge. Node is checked-in: " + safeGetJCRPath();
            log.error(msg);
            throw new VersionException(msg);
        }

        // check if version is in mergeFailed list
        Set failed = internalGetMergeFailed();
        if (!failed.remove(version.getUUID())) {
            String msg = "Unable to finish merge. Specified version is not in jcr:mergeFailed property: " + safeGetJCRPath();
            log.error(msg);
            throw new VersionException(msg);
        }

        // remove version from mergeFailed list
        internalSetMergeFailed(failed);

        if (!cancel) {
            // add version to jcr:predecessors list
            Value[] vals = getProperty(NameConstants.JCR_PREDECESSORS).getValues();
            InternalValue[] v = new InternalValue[vals.length + 1];
            for (int i = 0; i < vals.length; i++) {
                v[i] = InternalValue.create(UUID.fromString(vals[i].getString()));
            }
            v[vals.length] = InternalValue.create(UUID.fromString(version.getUUID()));
            internalSetProperty(NameConstants.JCR_PREDECESSORS, v);
        }

        // save
        save();
    }

    /**
     * @return
     * @throws RepositoryException
     */
    private Set internalGetMergeFailed() throws RepositoryException {
        HashSet set = new HashSet();
        if (hasProperty(NameConstants.JCR_MERGEFAILED)) {
            Value[] vals = getProperty(NameConstants.JCR_MERGEFAILED).getValues();
            for (int i = 0; i < vals.length; i++) {
                set.add(vals[i].getString());
            }
        }
        return set;
    }

    /**
     * @param set
     * @throws RepositoryException
     */
    private void internalSetMergeFailed(Set set) throws RepositoryException {
        if (set.isEmpty()) {
            internalSetProperty(NameConstants.JCR_MERGEFAILED, (InternalValue[]) null);
        } else {
            InternalValue[] vals = new InternalValue[set.size()];
            Iterator iter = set.iterator();
            int i = 0;
            while (iter.hasNext()) {
                String uuid = (String) iter.next();
                vals[i++] = InternalValue.create(UUID.fromString(uuid));
            }
            internalSetProperty(NameConstants.JCR_MERGEFAILED, vals);
        }
    }

    /**
     * Determines the checked-out status of this node.
     * <p/>
     * A node is considered <i>checked-out</i> if it is versionable and
     * checked-out, or is non-versionable but its nearest versionable ancestor
     * is checked-out, or is non-versionable and there are no versionable
     * ancestors.
     *
     * @return a boolean
     * @see Node#isCheckedOut()
     */
    protected boolean internalIsCheckedOut() throws RepositoryException {
        /**
         * try shortcut first:
         * if current node is 'new' we can safely consider it checked-out
         * since otherwise it would had been impossible to add it in the first
         * place
         */
        if (isNew()) {
            return true;
        }

        // search nearest ancestor that is versionable
        /**
         * FIXME should not only rely on existence of jcr:isCheckedOut property
         * but also verify that node.isNodeType("mix:versionable")==true;
         * this would have a negative impact on performance though...
         */
        NodeImpl node = this;
        while (!node.hasProperty(NameConstants.JCR_ISCHECKEDOUT)) {
            if (node.getDepth() == 0) {
                return true;
            }
            node = (NodeImpl) node.getParent();
        }
        return node.getProperty(NameConstants.JCR_ISCHECKEDOUT).getBoolean();
    }

    /**
     * Creates a new node at <code>relPath</code> of the node type, uuid and
     * eventual mixin types stored in the frozen node. The same as
     * <code>{@link #addNode(String relPath)}</code> except that the primary
     * node type type, the uuid and evt. mixin type of the new node is
     * explictly specified in the nt:frozen node.
     * <p/>
     *
     * @param name   The name of the new <code>Node</code> that is to be created.
     * @param frozen The frozen node that contains the creation information
     * @return The node that was added.
     * @throws ItemExistsException          If an item at the
     *                                      specified path already exists(and same-name siblings are not allowed).
     * @throws PathNotFoundException        If specified path implies intermediary
     *                                      <code>Node</code>s that do not exist.
     * @throws NoSuchNodeTypeException      If the specified <code>nodeTypeName</code>
     *                                      is not recognized.
     * @throws ConstraintViolationException If an attempt is made to add a node as the
     *                                      child of a <code>Property</code>
     * @throws RepositoryException          if another error occurs.
     */
    private NodeImpl addNode(Name name, InternalFrozenNode frozen)
            throws ItemExistsException, PathNotFoundException,
            ConstraintViolationException, NoSuchNodeTypeException,
            RepositoryException {

        // get frozen node type
        NodeTypeManagerImpl ntMgr = session.getNodeTypeManager();
        NodeTypeImpl nt = ntMgr.getNodeType(frozen.getFrozenPrimaryType());

        // get frozen uuid
        UUID uuid = frozen.getFrozenUUID();

        NodeImpl node = internalAddChildNode(name, nt, new NodeId(uuid));

        // get frozen mixin
        // todo: also respect mixing types on creation?
        Name[] mxNames = frozen.getFrozenMixinTypes();
        for (int i = 0; i < mxNames.length; i++) {
            node.addMixin(mxNames[i]);
        }
        return node;
    }

    /**
     * Creates a new node at <code>relPath</code> of the node type, uuid and
     * eventual mixin types stored in the frozen node. The same as
     * <code>{@link #addNode(String relPath)}</code> except that the primary
     * node type type, the uuid and evt. mixin type of the new node is
     * explictly specified in the nt:frozen node.
     * <p/>
     *
     * @param relPath The path of the new <code>Node</code> that is to be created.
     * @param frozen  The frozen node that contains the creation information
     * @return The node that was added.
     * @throws ItemExistsException          If an item at the
     *                                      specified path already exists(and same-name siblings are not allowed).
     * @throws PathNotFoundException        If specified path implies intermediary
     *                                      <code>Node</code>s that do not exist.
     * @throws NoSuchNodeTypeException      If the specified <code>nodeTypeName</code>
     *                                      is not recognized.
     * @throws ConstraintViolationException If an attempt is made to add a node as the
     *                                      child of a <code>Property</code>
     * @throws RepositoryException          if another error occurs.
     */
    private NodeImpl addNode(String relPath, InternalFrozenNode frozen)
            throws ItemExistsException, PathNotFoundException,
            ConstraintViolationException, NoSuchNodeTypeException,
            RepositoryException {

        // get frozen node type
        NodeTypeManagerImpl ntMgr = session.getNodeTypeManager();
        NodeTypeImpl nt = ntMgr.getNodeType(frozen.getFrozenPrimaryType());

        // get frozen uuid
        UUID uuid = frozen.getFrozenUUID();

        NodeImpl node = internalAddNode(relPath, nt, new NodeId(uuid));

        // get frozen mixin
        // todo: also respect mixing types on creation?
        Name[] mxNames = frozen.getFrozenMixinTypes();
        for (int i = 0; i < mxNames.length; i++) {
            node.addMixin(mxNames[i]);
        }
        return node;
    }

    /**
     * {@inheritDoc}
     */
    private void internalMerge(String srcWorkspaceName,
                               List failedIds, boolean bestEffort)
            throws NoSuchWorkspaceException, AccessDeniedException,
            LockException, InvalidItemStateException, RepositoryException {

        // might be added in future releases
        boolean removeExisting = true;
        boolean replaceExisting = false;

        // do checks
        sanityCheck();
        checkSessionHasPending();

        // if same workspace, ignore
        if (srcWorkspaceName.equals(session.getWorkspace().getName())) {
            return;
        }

        SessionImpl srcSession = null;
        try {
            // create session on other workspace for current subject
            // (may throw NoSuchWorkspaceException and AccessDeniedException)
            srcSession = rep.createSession(session.getSubject(), srcWorkspaceName);
            try {
                internalMerge(srcSession, failedIds, bestEffort, removeExisting, replaceExisting);
            } catch (RepositoryException e) {
                try {
                    session.refresh(false);
                } catch (RepositoryException e1) {
                    // ignore
                }
                throw e;
            }
            session.save();
        } finally {
            if (srcSession != null) {
                // we don't need the other session anymore, logout
                srcSession.logout();
            }
        }
    }

    /**
     * Merges/Updates this node with its corresponding ones
     *
     * @param srcSession
     * @param failedIds
     * @param bestEffort
     * @param removeExisting
     * @param replaceExisting
     * @throws LockException
     * @throws RepositoryException
     */
    private void internalMerge(SessionImpl srcSession, List failedIds,
                               boolean bestEffort, boolean removeExisting, boolean replaceExisting)
            throws LockException, RepositoryException {

        NodeImpl srcNode = doMergeTest(srcSession, failedIds, bestEffort);
        if (srcNode == null) {
            // leave, iterate over children, but ignore non-versionable child
            // nodes (see JCR-1046)
            NodeIterator iter = getNodes();
            while (iter.hasNext()) {
                NodeImpl n = (NodeImpl) iter.nextNode();
                if (n.isNodeType(NameConstants.MIX_VERSIONABLE)) {
                    n.internalMerge(srcSession, failedIds, bestEffort, removeExisting, replaceExisting);
                }
            }
            return;
        }

        // check lock status
        checkLock();

        // update the properties
        PropertyIterator iter = getProperties();
        while (iter.hasNext()) {
            PropertyImpl p = (PropertyImpl) iter.nextProperty();
            if (!srcNode.hasProperty(p.getQName())) {
                p.internalRemove(true);
            }
        }
        iter = srcNode.getProperties();
        while (iter.hasNext()) {
            PropertyImpl p = (PropertyImpl) iter.nextProperty();
            // ignore system types
            if (p.getQName().equals(NameConstants.JCR_PRIMARYTYPE)
                    || p.getQName().equals(NameConstants.JCR_MIXINTYPES)
                    || p.getQName().equals(NameConstants.JCR_UUID)) {
                continue;
            }
            if (p.getDefinition().isMultiple()) {
                internalSetProperty(p.getQName(), p.internalGetValues());
            } else {
                internalSetProperty(p.getQName(), p.internalGetValue());
            }
        }
        // todo: add/remove mixins ?

        // update the nodes. remove non existing ones
        NodeIterator niter = getNodes();
        while (niter.hasNext()) {
            NodeImpl n = (NodeImpl) niter.nextNode();
            Path.Element name = n.getPrimaryPath().getNameElement();
            int idx = name.getIndex();
            if (idx == 0) {
                idx = 1;
            }
            if (!srcNode.hasNode(name.getName(), idx)) {
                n.internalRemove(true);
            }
        }

        // add source ones
        niter = srcNode.getNodes();
        while (niter.hasNext()) {
            NodeImpl child = (NodeImpl) niter.nextNode();
            NodeImpl dstNode = null;
            NodeId childId = child.getNodeId();
            Path.Element name = child.getPrimaryPath().getNameElement();
            int idx = name.getIndex();
            if (idx == 0) {
                idx = 1;
            }

            if (child.isNodeType(NameConstants.MIX_REFERENCEABLE)) {
                // check if correspondance exist in
                // this workspace
                try {
                    dstNode = session.getNodeById(childId);
                    // check if same parent
                    if (!isSame(dstNode.getParent())) {
                        if (removeExisting) {
                            dstNode.internalRemove(false);
                            dstNode = null;
                        } else if (replaceExisting) {
                            // node exists outside of this update tree, so continue there
                        } else {
                            throw new ItemExistsException("Unable to update node: " + dstNode.safeGetJCRPath());
                        }
                    }

                } catch (ItemNotFoundException e) {
                    // does not exist
                }
            } else {
                // if child is not referenceable, clear uuid
                childId = null;
            }
            if (dstNode == null && hasNode(name.getName(), idx)) {
                // the exact behaviour for SNS is not specified by the spec
                // so we just try to find the corresponding one.
                dstNode = getNode(name.getName(), idx);
            }
            if (dstNode == null) {
                dstNode = internalAddChildNode(name.getName(), (NodeTypeImpl) child.getPrimaryNodeType(), childId);
                // add mixins
                NodeType[] mixins = child.getMixinNodeTypes();
                for (int i = 0; i < mixins.length; i++) {
                    dstNode.addMixin(mixins[i].getName());
                }
                dstNode.internalMerge(srcSession, null, bestEffort, removeExisting, replaceExisting);
            } else {
                dstNode.internalMerge(srcSession, failedIds, bestEffort, removeExisting, replaceExisting);
            }
        }
    }

    /**
     * Internal method to restore a version.
     *
     * @param version
     * @param vsel    the version selector that will select the correct version for
     *                OPV=Version childnodes.
     * @throws UnsupportedRepositoryOperationException
     *
     * @throws RepositoryException
     */
    private void internalRestore(Version version, VersionSelector vsel, boolean removeExisting)
            throws UnsupportedRepositoryOperationException, RepositoryException {

        try {
            internalRestore((VersionImpl) version, vsel, removeExisting);
        } catch (RepositoryException e) {
            // revert session
            try {
                log.error("reverting changes applied during restore...");
                session.refresh(false);
            } catch (RepositoryException e1) {
                // ignore this
            }
            throw e;
        }
        session.save();
    }

    /**
     * Internal method to restore a version.
     *
     * @param version
     * @param vsel           the version selector that will select the correct version for
     *                       OPV=Version childnodes.
     * @param removeExisting
     * @throws RepositoryException
     */
    protected Version[] internalRestore(VersionImpl version, VersionSelector vsel,
                                        boolean removeExisting)
            throws RepositoryException {

        // fail if root version
        if (version.isRootVersion()) {
            throw new VersionException("Restore of root version not allowed.");
        }

        // set jcr:isCheckedOut property to true, in order to avoid any conflicts
        internalSetProperty(NameConstants.JCR_ISCHECKEDOUT, InternalValue.create(true));

        // 1. The child node and properties of N will be changed, removed or
        //    added to, depending on their corresponding copies in V and their
        //    own OnParentVersion attributes (see 7.2.8, below, for details).
        HashSet restored = new HashSet();
        restoreFrozenState(version.getFrozenNode(), vsel, restored, removeExisting);
        restored.add(version);

        // 2. N's jcr:baseVersion property will be changed to point to V.
        UUID uuid = ((NodeId) version.getId()).getUUID();
        internalSetProperty(NameConstants.JCR_BASEVERSION, InternalValue.create(uuid));

        // 4. N's jcr:predecessor property is set to null
        internalSetProperty(NameConstants.JCR_PREDECESSORS, InternalValue.EMPTY_ARRAY, PropertyType.REFERENCE);

        // also clear mergeFailed
        internalSetProperty(NameConstants.JCR_MERGEFAILED, (InternalValue[]) null);

        // 3. N's jcr:isCheckedOut property is set to false.
        internalSetProperty(NameConstants.JCR_ISCHECKEDOUT, InternalValue.create(false));

        return (Version[]) restored.toArray(new Version[restored.size()]);
    }

    /**
     * Restores the properties and child nodes from the frozen state.
     *
     * @param freeze
     * @param vsel
     * @param removeExisting
     * @throws RepositoryException
     */
    void restoreFrozenState(InternalFrozenNode freeze, VersionSelector vsel, Set restored, boolean removeExisting)
            throws RepositoryException {

        // check uuid
        if (isNodeType(NameConstants.MIX_REFERENCEABLE)) {
            UUID uuid = freeze.getFrozenUUID();
            if (!internalGetUUID().equals(uuid)) {
                throw new ItemExistsException("Unable to restore version of " + safeGetJCRPath() + ". UUID changed.");
            }
        }

        // check primary type
        if (!freeze.getFrozenPrimaryType().equals(primaryTypeName)) {
            // todo: check with spec what should happen here
            throw new ItemExistsException("Unable to restore version of " + safeGetJCRPath() + ". PrimaryType changed.");
        }

        // adjust mixins
        Name[] mixinNames = freeze.getFrozenMixinTypes();
        setMixinTypesProperty(new HashSet(Arrays.asList(mixinNames)));

        // copy frozen properties
        PropertyState[] props = freeze.getFrozenProperties();
        HashSet propNames = new HashSet();
        for (int i = 0; i < props.length; i++) {
            PropertyState prop = props[i];
            propNames.add(prop.getName());
            if (prop.isMultiValued()) {
                internalSetProperty(
                        props[i].getName(), prop.getValues(), prop.getType());
            } else {
                internalSetProperty(props[i].getName(), prop.getValues()[0]);
            }
        }
        // remove properties that do not exist in the frozen representation
        PropertyIterator piter = getProperties();
        while (piter.hasNext()) {
            PropertyImpl prop = (PropertyImpl) piter.nextProperty();
            // ignore some props that are not well guarded by the OPV
            if (prop.getQName().equals(NameConstants.JCR_VERSIONHISTORY)) {
                continue;
            } else if (prop.getQName().equals(NameConstants.JCR_PREDECESSORS)) {
                continue;
            }
            if (prop.getDefinition().getOnParentVersion() == OnParentVersionAction.COPY
                    || prop.getDefinition().getOnParentVersion() == OnParentVersionAction.VERSION) {
                if (!propNames.contains(prop.getQName())) {
                    removeChildProperty(prop.getQName());
                }
            }
        }

        // add 'auto-create' properties that do not exist yet
        NodeTypeManagerImpl ntMgr = session.getNodeTypeManager();
        for (int j = 0; j < mixinNames.length; j++) {
            NodeTypeImpl mixin = ntMgr.getNodeType(mixinNames[j]);
            PropertyDefinition[] pda = mixin.getAutoCreatedPropertyDefinitions();
            for (int i = 0; i < pda.length; i++) {
                PropertyDefinitionImpl pd = (PropertyDefinitionImpl) pda[i];
                if (!hasProperty(pd.getQName())) {
                    createChildProperty(pd.getQName(), pd.getRequiredType(), pd);
                }
            }
        }

        // first delete some of the version histories
        NodeIterator iter = getNodes();
        while (iter.hasNext()) {
            NodeImpl n = (NodeImpl) iter.nextNode();
            if (n.getDefinition().getOnParentVersion() == OnParentVersionAction.COPY) {
                // only remove OPV=Copy nodes
                n.internalRemove(true);
            } else if (n.getDefinition().getOnParentVersion() == OnParentVersionAction.VERSION) {
                // only remove, if node to be restored does not contain child,
                // or if restored child is not versionable
                UUID vhUUID = n.hasProperty(NameConstants.JCR_VERSIONHISTORY)
                        ? new UUID(n.getProperty(NameConstants.JCR_VERSIONHISTORY).getString())
                        : null;
                if (vhUUID == null || !freeze.hasFrozenHistory(vhUUID)) {
                    n.internalRemove(true);
                }
            }
        }

        // restore the frozen nodes
        InternalFreeze[] frozenNodes = freeze.getFrozenChildNodes();
        for (int i = 0; i < frozenNodes.length; i++) {
            InternalFreeze child = frozenNodes[i];
            NodeImpl restoredChild = null;
            if (child instanceof InternalFrozenNode) {
                InternalFrozenNode f = (InternalFrozenNode) child;
                // check for existing
                if (f.getFrozenUUID() != null) {
                    try {
                        NodeImpl existing = (NodeImpl) session.getNodeByUUID(f.getFrozenUUID());
                        // check if one of this restoretrees node
                        if (removeExisting) {
                            existing.remove();
                        } else {
                            // since we delete the OPV=Copy children beforehand, all
                            // found nodes must be outside of this tree
                            throw new ItemExistsException("Unable to restore node, item already exists outside of restored tree: "
                                    + existing.safeGetJCRPath());
                        }
                    } catch (ItemNotFoundException e) {
                        // ignore, item with uuid does not exist
                    }
                }
                restoredChild = addNode(f.getName(), f);
                restoredChild.restoreFrozenState(f, vsel, restored, removeExisting);

            } else if (child instanceof InternalFrozenVersionHistory) {
                InternalFrozenVersionHistory f = (InternalFrozenVersionHistory) child;
                VersionHistory history = (VersionHistory) session.getNodeById(f.getVersionHistoryId());
                NodeId nodeId = NodeId.valueOf(history.getVersionableUUID());
                String oldVersion = "jcr:dummy";

                // check if representing versionable already exists somewhere
                if (itemMgr.itemExists(nodeId)) {
                    NodeImpl n = session.getNodeById(nodeId);
                    if (removeExisting) {
                        String dstPath = getPath() + "/" + n.getName();
                        if (!n.getPath().equals(dstPath)) {
                            session.move(n.getPath(), dstPath);
                        }
                        oldVersion = n.getBaseVersion().getName();
                    } else if (n.getParent().isSame(this)) {
                        n.internalRemove(true);
                    } else {
                        // since we delete the OPV=Copy children beforehand, all
                        // found nodes must be outside of this tree
                        throw new ItemExistsException("Unable to restore node, item already exists outside of restored tree: "
                                + n.safeGetJCRPath());
                    }
                }
                // get desired version from version selector
                VersionImpl v = (VersionImpl) vsel.select(history);

                // check existing version of item exists
                if (!itemMgr.itemExists(nodeId)) {
                    if (v == null) {
                        // if version selector was unable to select version,
                        // choose the initial one
                        Version[] vs = history.getRootVersion().getSuccessors();
                        if (vs.length == 0) {
                            String msg = "Unable to select appropariate version for " +
                                    child.getName() + " using " + vsel;
                            log.error(msg);
                            throw new VersionException(msg);
                        }
                        v = (VersionImpl) vs[0];
                    }
                    restoredChild = addNode(child.getName(), v.getFrozenNode());
                } else {
                    restoredChild = session.getNodeById(nodeId);
                    if (v == null || oldVersion == null || v.getName().equals(oldVersion)) {
                        v = null;
                    }
                }
                if (v != null) {
                    try {
                        restoredChild.internalRestore(v, vsel, removeExisting);
                    } catch (RepositoryException e) {
                        log.error("Error while restoring node: " + e.toString());
                        log.error("  child path: " + restoredChild.safeGetJCRPath());
                        log.error("  selected version: " + v.getName());
                        StringBuffer avail = new StringBuffer();
                        VersionIterator vi = history.getAllVersions();
                        while (vi.hasNext()) {
                            avail.append(vi.nextVersion().getName());
                            if (vi.hasNext()) {
                                avail.append(", ");
                            }
                        }
                        log.error("  available versions: " + avail);
                        log.error("  versionselector: " + vsel);
                        throw e;
                    }
                    // add this version to set
                    restored.add(v);
                }
            }
            // ensure proper odering (issue JCR-469)
            if (restoredChild != null
                    && getPrimaryNodeType().hasOrderableChildNodes()) {
                orderBefore(restoredChild.getPrimaryPath().getNameElement(), null);
            }
        }
    }

    /**
     * Copies a property to this node
     *
     * @param prop
     * @throws RepositoryException
     */
    protected void internalCopyPropertyFrom(PropertyImpl prop) throws RepositoryException {
        if (prop.getDefinition().isMultiple()) {
            Value[] values = prop.getValues();
            InternalValue[] ivalues = new InternalValue[values.length];
            for (int i = 0; i < values.length; i++) {
                ivalues[i] = InternalValue.create(values[i], session.getNamePathResolver(), rep.getDataStore());
            }
            internalSetProperty(prop.getQName(), ivalues);
        } else {
            InternalValue value = InternalValue.create(prop.getValue(), session.getNamePathResolver(), rep.getDataStore());
            internalSetProperty(prop.getQName(), value);
        }
    }

    //------------------------------------------------------< locking support >
    /**
     * {@inheritDoc}
     */
    public Lock lock(boolean isDeep, boolean isSessionScoped)
            throws UnsupportedRepositoryOperationException, LockException,
            AccessDeniedException, InvalidItemStateException,
            RepositoryException {
        // check state of this instance
        sanityCheck();

        // check for pending changes
        if (hasPendingChanges()) {
            String msg = "Unable to lock node. Node has pending changes: " + safeGetJCRPath();
            log.debug(msg);
            throw new InvalidItemStateException(msg);
        }

        checkLockable();

        LockManager lockMgr = session.getLockManager();
        synchronized (lockMgr) {
            Lock lock = lockMgr.lock(this, isDeep, isSessionScoped);

            boolean succeeded = false;

            try {
                // add properties to content
                internalSetProperty(NameConstants.JCR_LOCKOWNER,
                        InternalValue.create(getSession().getUserID()));
                internalSetProperty(NameConstants.JCR_LOCKISDEEP,
                        InternalValue.create(isDeep));
                save();
                succeeded = true;
            } finally {
                if (!succeeded) {
                    // failed to set lock meta-data content, cleanup
                    try {
                        lockMgr.unlock(this);
                        refresh(false);
                    } catch (RepositoryException re) {
                        // cleanup failed
                        log.error("error while cleaning up after failed lock attempt", re);
                    }
                }
            }

            return lock;
        }
    }

    /**
     * {@inheritDoc}
     */
    public Lock getLock()
            throws UnsupportedRepositoryOperationException, LockException,
            AccessDeniedException, RepositoryException {
        // check state of this instance
        sanityCheck();

        if (isNew()) {
            throw new LockException("Node not locked: " + safeGetJCRPath());
        }
        return session.getLockManager().getLock(this);
    }

    /**
     * {@inheritDoc}
     */
    public void unlock()
            throws UnsupportedRepositoryOperationException, LockException,
            AccessDeniedException, InvalidItemStateException,
            RepositoryException {
        // check state of this instance
        sanityCheck();

        // check for pending changes
        if (hasPendingChanges()) {
            String msg = "Unable to unlock node. Node has pending changes: " + safeGetJCRPath();
            log.debug(msg);
            throw new InvalidItemStateException(msg);
        }

        checkLockable();

        LockManager lockMgr = session.getLockManager();
        synchronized (lockMgr) {
            if (lockMgr.isLockHolder(session, this)) {
                // save first, and unlock later. this guards concurrent access

                // remove properties in content
                internalSetProperty(NameConstants.JCR_LOCKOWNER, (InternalValue) null);
                internalSetProperty(NameConstants.JCR_LOCKISDEEP, (InternalValue) null);
                save();
            }
            lockMgr.unlock(this);
        }
    }

    /**
     * {@inheritDoc}
     */
    public boolean holdsLock() throws RepositoryException {
        // check state of this instance
        sanityCheck();

        if (!isNodeType(NameConstants.MIX_LOCKABLE) || isNew()) {
            // a node that is new or not lockable never holds a lock
            return false;
        }
        return session.getLockManager().holdsLock(this);
    }

    /**
     * {@inheritDoc}
     */
    public boolean isLocked() throws RepositoryException {
        // check state of this instance
        sanityCheck();

        if (isNew()) {
            return false;
        }
        return session.getLockManager().isLocked(this);
    }

    /**
     * Checks if this node is lockable, i.e. has 'mix:lockable'.
     *
     * @throws LockException       if this node is not lockable
     * @throws RepositoryException if another error occurs
     */
    private void checkLockable() throws LockException, RepositoryException {
        if (!isNodeType(NameConstants.MIX_LOCKABLE)) {
            String msg = "Unable to perform locking operation on non-lockable node: "
                    + safeGetJCRPath();
            log.debug(msg);
            throw new LockException(msg);
        }
    }

    /**
     * Check whether this node is locked by somebody else.
     *
     * @throws LockException       if this node is locked by somebody else
     * @throws RepositoryException if some other error occurs
     */
    protected void checkLock() throws LockException, RepositoryException {
        if (isNew()) {
            // a new node needs no check
            return;
        }
        session.getLockManager().checkLock(this);
    }

    //--------------------------------------------------< new JSR 283 methods >
    /**
     * Returns the identifier of this node. Applies to both referenceable and
     * non-referenceable nodes.
     * <p/>
     * A <code>RepositoryException</code> is thrown if an error occurs.
     *
     * @return the identifier of this node
     * @throws RepositoryException If an error occurs.
     * @since JCR 2.0
     */
    public String getIdentifier() throws RepositoryException {
        return ((NodeId) id).toString();
    }

    /**
     * This method returns all <code>REFERENCE</code> properties that refer to
     * this node, have the specified <code>name</code> and that are accessible
     * through the current <code>Session</code>.
     * <p/>
     * If the <code>name</code> parameter is <code>null</code> then all
     * referring <code>REFERENCES</code> are returned regardless of name.
     * <p/>
     * Some level 2 implementations may only return properties that have been
     * saved (in a transactional setting this includes both those properties
     * that have been saved but not yet committed, as well as properties that
     * have been committed). Other level 2 implementations may additionally
     * return properties that have been added within the current <code>Session</code>
     * but are not yet saved.
     * <p/>
     * In implementations that support versioning, this method does not return
     * properties that are part of the frozen state of a version in version
     * storage.
     * <p/>
     * If this node has no referring properties with the specified name,
     * an empty iterator is returned.
     *
     * @param name name of referring <code>REFERENCE</code> properties to be
     *        returned; if <code>null</code> then all referring <code>REFERENCE</code>s
     *        are returned
     * @return A <code>PropertyIterator</code>.
     * @throws RepositoryException if an error occurs
     * @since JCR 2.0
     */
    public PropertyIterator getReferences(String name)
            throws RepositoryException {
        // check state of this instance
        sanityCheck();

        try {
            NodeReferencesId targetId = new NodeReferencesId((NodeId) id);
            if (stateMgr.hasNodeReferences(targetId)) {
                NodeReferences refs = stateMgr.getNodeReferences(targetId);
                // refs.getReferences() returns a list of PropertyId's
                List idList = refs.getReferences();
                if (name != null) {
                    Name qName;
                    try {
                        qName = session.getQName(name);
                    } catch (NameException e) {
                        throw new RepositoryException("invalid property name: " + name, e);
                    }
                    ArrayList filteredList = new ArrayList(idList.size());
                    for (Iterator iter = idList.iterator(); iter.hasNext();) {
                        PropertyId propId = (PropertyId) iter.next();
                        if (propId.getName().equals(qName)) {
                            filteredList.add(propId);
                        }
                    }
                    idList = filteredList;
                }
                return new LazyItemIterator(itemMgr, idList);
            } else {
                // there are no references, return empty iterator
                return PropertyIteratorAdapter.EMPTY;
            }
        } catch (ItemStateException e) {
            String msg = "Unable to retrieve REFERENCE properties that refer to " + id;
            log.debug(msg);
            throw new RepositoryException(msg, e);
        }
    }

    /**
     * Changes the primary node type of this node to <code>nodeTypeName</code>.
     * Also immediately changes this node's <code>jcr:primaryType</code> property
     * appropriately. Semantically, the new node type may take effect
     * immediately and <i>must</i> take effect on <code>save</code>. Whichever
     * behavior is adopted it must be the same as the behavior adopted for
     * <code>addMixin()</code> (see below) and the behavior that occurs when a
     * node is first created.
     * <p/>
     * If the presence of an existing property or child node would cause an
     * incompatibility with the new node type a <code>ConstraintViolationException</code>
     * is thrown either immediately or on <code>save</code>.
     * <p/>
     * If the new node type would cause this node to be incompatible with the
     * node type of its parent then a <code>ConstraintViolationException</code>
     * is thrown either immediately or on <code>save</code>.
     * <p/>
     * A <code>ConstraintViolationException</code> is also thrown either
     * immediately or on <code>save</code> if a conflict with an already
     * assigned mixin occurs.
     * <p/>
     * A <code>ConstraintViolationException</code> may also be thrown either
     * immediately or on <code>save</code> if the attempted change violates
     * implementation-specific node type transition rules. A repository that
     * disallows all primary node type changes would simple throw this
     * exception in all cases.
     * <p/>
     * If the specified node type is not recognized a
     * <code>NoSuchNodeTypeException</code> is thrown either immediately
     * or on <code>save</code>.
     * <p/>
     * A <code>VersionException</code> is thrown either immediately or on
     * <code>save</code> if this node is versionable and checked-in, or is
     * non-versionable but its nearest versionable ancestor is checked-in.
     * <p/>
     * A <code>LockException</code> is thrown either immediately or on
     * <code>save</code> if a lock prevents the change of node type.
     * <p/>
     * A <code>RepositoryException</code> will be thrown if another error occurs.
     *
     * @param nodeTypeName the name of the new node type.
     * @throws ConstraintViolationException If the specified primary node type
     * is prevented from being assigned.
     * @throws NoSuchNodeTypeException If the specified <code>nodeTypeName</code>
     * is not recognized and this implementation performs this validation
     * immediately instead of waiting until <code>save</code>.
     * @throws VersionException if this node is versionable and checked-in or is
     * non-versionable but its nearest versionable ancestor is checked-in and this
     * implementation performs this validation immediately instead of waiting until
     * <code>save</code>.
     * @throws LockException if a lock prevents the change of the primary node type
     * and this implementation performs this validation immediately instead of
     * waiting until <code>save</code>.
     * @throws RepositoryException  if another error occurs.
     * @since JCR 2.0
     */
    public void setPrimaryType(String nodeTypeName)
            throws NoSuchNodeTypeException, VersionException,
            ConstraintViolationException, LockException, RepositoryException {
        // check state of this instance
        sanityCheck();

        // make sure this node is checked-out
        if (!internalIsCheckedOut()) {
            String msg = safeGetJCRPath() + ": cannot set primary type of a checked-in node";
            log.debug(msg);
            throw new VersionException(msg);
        }

        // check protected flag
        if (definition.isProtected()) {
            String msg = safeGetJCRPath() + ": cannot set primary type of a protected node";
            log.debug(msg);
            throw new ConstraintViolationException(msg);
        }

        if (state.getParentId() == null) {
            String msg = "changing the primary type of the root node is not supported";
            log.debug(msg);
            throw new RepositoryException(msg);
        }

        // check lock status
        checkLock();

        Name ntName;
        try {
            ntName = session.getQName(nodeTypeName);
        } catch (NameException e) {
            throw new RepositoryException(
                    "invalid node type name: " + nodeTypeName, e);
        }

        if (ntName.equals(primaryTypeName)) {
            return;
        }

        NodeTypeManagerImpl ntMgr = session.getNodeTypeManager();
        if (ntMgr.getNodeType(ntName).isMixin()) {
            throw new RepositoryException(nodeTypeName + ": not a primary node type");
        }

        // build effective node type of new primary type & existing mixin's
        // in order to detect conflicts
        NodeTypeRegistry ntReg = ntMgr.getNodeTypeRegistry();
        EffectiveNodeType entNew, entOld;
        try {
            entNew = ntReg.getEffectiveNodeType(ntName);
            entOld = ntReg.getEffectiveNodeType(primaryTypeName);

            // existing mixin's
            HashSet set = new HashSet(((NodeState) state).getMixinTypeNames());
            // new primary type
            set.add(ntName);
            // try to build new effective node type (will throw in case of conflicts)
            ntReg.getEffectiveNodeType((Name[]) set.toArray(new Name[set.size()]));
        } catch (NodeTypeConflictException ntce) {
            throw new ConstraintViolationException(ntce.getMessage());
        }

        // get applicable definition for this node using new primary type
        NodeDefId defId;
        try {
            NodeImpl parent = (NodeImpl) getParent();
            defId = parent.getApplicableChildNodeDefinition(getQName(), ntName).unwrap().getId();
        } catch (RepositoryException re) {
            String msg = safeGetJCRPath() + ": no applicable definition found in parent node's node type";
            log.debug(msg);
            throw new ConstraintViolationException(msg, re);
        }
        
        if (!defId.equals(((NodeState) state).getDefinitionId())) {
            onRedefine(defId);
        }


        // build change set: removed/added child items
        Set oldDefs = new HashSet(Arrays.asList(entOld.getAllItemDefs()));
        Set newDefs = new HashSet(Arrays.asList(entNew.getAllItemDefs()));

        Set removedDefs = new HashSet(oldDefs);
        removedDefs.removeAll(newDefs);

        Set addedDefs = new HashSet(newDefs);
        addedDefs.removeAll(oldDefs);

        // referential integrity check
        boolean referenceableOld = entOld.includesNodeType(NameConstants.MIX_REFERENCEABLE);
        boolean referenceableNew = entNew.includesNodeType(NameConstants.MIX_REFERENCEABLE);
        if (referenceableOld && !referenceableNew) {
            // node would become non-referenceable;
            // make sure no references exist
            PropertyIterator iter = getReferences();
            if (iter.hasNext()) {
                throw new ConstraintViolationException(
                        "the new primary type cannot be set as it would render "
                                + "this node 'non-referenceable' while it is still being "
                                + "referenced through at least one property of type REFERENCE");
            }
        }

        // do the actual modifications in content as mandated by the new primary type

        // modify the state of this node
        NodeState thisState = (NodeState) getOrCreateTransientItemState();
        thisState.setNodeTypeName(ntName);

        // set jcr:primaryType property
        internalSetProperty(NameConstants.JCR_PRIMARYTYPE, InternalValue.create(ntName));

        // walk through properties and child nodes and remove those that
        // are not included in the new node type
        if (!removedDefs.isEmpty()) {
            // use temp set to avoid ConcurrentModificationException
            HashSet set = new HashSet(thisState.getPropertyNames());
            for (Iterator iter = set.iterator(); iter.hasNext();) {
                Name propName = (Name) iter.next();
                try {
                    PropertyState propState =
                            (PropertyState) stateMgr.getItemState(
                                    new PropertyId(thisState.getNodeId(), propName));
                    if (removedDefs.contains(ntReg.getPropDef(propState.getDefinitionId()))) {
                        removeChildProperty(propName);
                    }
                } catch (ItemStateException ise) {
                    String msg = propName + ": failed to retrieve property state";
                    log.error(msg, ise);
                    throw new RepositoryException(msg, ise);
                }
            }
            // use temp array to avoid ConcurrentModificationException
            ArrayList list = new ArrayList(thisState.getChildNodeEntries());
            // start from tail to avoid problems with same-name siblings
            for (int i = list.size() - 1; i >= 0; i--) {
                NodeState.ChildNodeEntry entry = (NodeState.ChildNodeEntry) list.get(i);
                try {
                    NodeState nodeState =
                            (NodeState) stateMgr.getItemState(entry.getId());
                    if (removedDefs.contains(ntReg.getNodeDef(nodeState.getDefinitionId()))) {
                        removeChildNode(entry.getName(), entry.getIndex());
                    }
                } catch (ItemStateException ise) {
                    String msg = entry.getName() + ": failed to retrieve node state";
                    log.error(msg, ise);
                    throw new RepositoryException(msg, ise);
                }
            }
        }

        // create new 'auto-create' items
        for (Iterator iter = addedDefs.iterator(); iter.hasNext();) {
            ItemDef def = (ItemDef) iter.next();
            if (def.isAutoCreated()) {
                if (def.definesNode()) {
                    NodeDefinitionImpl nd = ntMgr.getNodeDefinition(((NodeDef) def).getId());
                    createChildNode(nd.getQName(), nd, (NodeTypeImpl) nd.getDefaultPrimaryType(), null);
                } else {
                    PropertyDefinitionImpl pd = ntMgr.getPropertyDefinition(((PropDef) def).getId());
                    createChildProperty(pd.getQName(), pd.getRequiredType(), pd);
                }
            }
        }
    }
}
