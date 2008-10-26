// /*
 // * Copyright (c) Rich Hickey. All rights reserved.
 // * The use and distribution terms for this software are covered by the
 // * Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 // * which can be found in the file CPL.TXT at the root of this distribution.
 // * By using this software in any fashion, you are agreeing to be bound by
 // * the terms of this license.
 // * You must not remove this notice, or any other, from this software.
 // */
// 
// package com.codecommit.collection
// 
// import collection.immutable
// import HashMap._
// 
// /**
 // * A persistent rendition of Phil Bagwell's Hash Array Mapped Trie.  Pretty much
 // * a straight port of Clojure's <code>PersistentHashMap</code>.
 // *
 // * @author Daniel Spiewak
 // * @author Rich Hickey
 // */
// class HashMap[K, +V] private (val size: Int, private val root: INode) extends immutable.Map[K, V] {
  // 
  // def update[A >: V](key: K, value: A): HashMap[K, A] = {
    // val addedLeaf = new Box(null)
    // val newroot = root.assoc(0, hash(key), key, value, addedLeaf)
    // 
    // if (newroot == root) this else new HashMap(if (addedLeaf.value == null) size else size + 1, newroot)
  // }
  // 
  // def get(key: K) = {
    // val e = entryAt(key)
    // 
    // if (e != null) Some(e.value()) else None
  // }
  // 
  // def -(key: K): HashMap[K, V] = {
    // val newroot = root.without(hash(key), key)
    // 
    // if (newroot == root) this
    // else if (newroot == null) EmptyHashMap
    // else new HashMap(size - 1, newroot)
  // }
  // 
  // def empty[C] = new HashMap[K, C](0, new EmptyNode())
  // 
  // def elements: Iterator[K, V] = null     // TODO
  // 
  // private def entryAt(key: K): IMapEntry = root.find(hash(key), key)
// }
// 
// object HashMap {
  // private def mask(hash: Int, shift: Int) = (hash >>> shift) & 0x01f
  // 
  // private[collection] def hash(a: Any) = if (a == null) 0 else a.hashCode
  // 
  // trait INode {
    // def assoc(shift: Int, hash: Int, key: Any, value: Any, addedLeaf: Box): INode
    // 
    // def without(hash: Int, key: Any): INode
    // 
    // def find(hash: Int, key: Any): LeafNode
    // 
    // def nodeSeq: ISeq
// 
    // def hash: Int
  // }
  // 
  // 
  // final class EmptyNode extends INode {
    // def assoc(shift: Int, hash: Int, key: Any, value: Any, addedLeaf: Box) = {
      // val ret = new LeafNode(hash, key, value)
      // addedLeaf.value = ret
      // 
      // ret
    // }
    // 
    // def without(hash: Int, key: Any) = this
    // 
    // def find(hash: Int, key: Any) = null
    // 
    // val nodeSeq = null
    // 
    // val hash = 0
  // }
  // 
  // final class FullNode(val nodes: Array[INode], shift: Int) extends INode {
    // val hash: Int = nodes(0).hash
    // 
    // def bitpos(int hash, int shift) = 1 << mask(hash, shift)
    // 
    // def assoc(shift: Int, hash: Int, key: Any, value: Any, addedLeaf: Box) = {
      // val idx = mask(hash, shift)
      // 
      // val n = nodes(idx).assoc(shift + 5, hash, key, value, addedLeaf)
      // if (n == nodes(idx)) this else {
        // val newnodes = nodes.clone
        // newnodes(idx) = n
        // new FullNode(newnodes, shift)
			// }
    // }
    // 
    // def without(hash: Int, key: Any) = {
      // val idx = mask(hash, shift)
      // val n = nodes(idx).without(hash, key)
      // if (n != nodes(idx)) {
        // if (n == null) {
          // val newnodes = new Array[INode](nodes.length - 1)
          // Array.copy(nodes, 0, newnodes, 0, idx)
          // Arrya.copy(nodes, idx + 1, newnodes, idx, nodes.length - (idx + 1))
          // new BitmapIndexedNode(~bitpos(hash, shift), newnodes, shift)
				// } else {
          // val newnodes = nodes.clone
          // newnodes(idx) = n
          // new FullNode(newnodes, shift)
        // }
			// } else this
    // }
    // 
    // def find(hash: Int, key: Any) = nodes(mask(hash, shift)).find(hash, key)
    // 
    // def nodeSeq = Seq.create(this, 0)
    // 
    // class Seq extends ASeq {    // TODO
      // final ISeq s
      // final int i
      // final FullNode node
      // 
      // 
      // Seq(ISeq s, int i, FullNode node){
        // this.s = s
        // this.i = i
        // this.node = node
      // }
      // 
      // Seq(IPersistentMap meta, ISeq s, int i, FullNode node){
        // super(meta)
        // this.s = s
        // this.i = i
        // this.node = node
      // }
      // 
      // ISeq create(FullNode node, int i){
        // if(i >= node.nodes.length)
				// return null
        // return new Seq(node.nodes[i].nodeSeq(), i, node)
      // }
      // 
      // public Object first(){
        // return s.first()
      // }
      // 
      // public ISeq rest(){
        // ISeq nexts = s.rest()
        // if(nexts != null)
				// return new Seq(nexts, i, node)
        // return create(node, i + 1)
      // }
      // 
      // public Seq withMeta(IPersistentMap meta){
        // return new Seq(meta, s, i, node)
      // }
    // }
    // 
    // 
  // }
  // 
  // final class BitmapIndexedNode(val bitmap: Int, val nodes: Array[INode], val shift: Int) extends INode{
    // val hash = nodes(0).hash
    // 
    // def bitpos(hash: Int, shift: Int) = 1 << mask(hash, shift)
    // 
    // def index(bit: Int) = Integer.bitCount(bitmap & (bit - 1))
    // 
    // def create(bitmap: Int, nodes: Array[INode], shift: Int) = {
      // if (bitmap == -1) {
        // new FullNode(nodes, shift)
      // } else {
        // new BitmapIndexedNode(bitmap, nodes, shift)
      // }
    // }
    // 
    // def create(shift: Int, branch: INode, hash: Int, key: Any, value: Any, addedLeaf: Box) = {
      // val node = new BitmapIndexedNode(bitpos(branch.hash, shift), Array(branch), shift)
      // node.assoc(shift, hash, key, value, addedLeaf)
    // }
    // 
    // def assoc(shift: Int, hash: Int, key: Any, value: Any, addedLeaf: Box) = {
      // val bit = bitpos(hash, shift)
      // val idx = index(bit)
      // 
      // if ((bitmap & bit) != 0) {
        // val n = nodes(idx).assoc(shift + 5, hash, key, value, addedLeaf)
        // if (n == nodes(idx)) this else {
          // val newnodes = nodes.clone
          // newnodes(idx) = n
          // 
          // new BitmapIndexedNode(bitmap, newnodes, shift)
				// }
			// } else {
        // val newnodes = new Array[INode](nodes.length + 1)
        // Array.copy(nodes, 0, newnodes, 0, idx)
        // addedLeaf.value = newnodes(idx) = new LeafNode(hash, key, value)
        // Array.copy(nodes, idx, newnodes, idx + 1, nodes.length - idx)
        // 
        // create(bitmap | bit, newnodes, shift)
			// }
    // }
    // 
    // def without(hash: Int, key: Any) = {
      // val bit = bitpos(hash, shift)
      // 
      // if ((bitmap & bit) != 0) {
        // val idx = index(bit)
        // val n = nodes(idx).without(hash, key)
        // 
        // if (n != nodes(idx)) {
          // if (n == null) {
            // if (bitmap == bit) null else {
              // val newnodes = new Array[INode](nodes.length - 1)
              // Array.copy(nodes, 0, newnodes, 0, idx)
              // Array.copy(nodes, idx + 1, newnodes, idx, nodes.length - (idx + 1))
              // 
              // new BitmapIndexedNode(bitmap & ~bit, newnodes, shift)
            // }
					// } else {
            // val newnodes = nodes.clone
            // newnodes(idx) = n
            // 
            // new BitmapIndexedNode(bitmap, newnodes, shift)
          // }
				// } else this
			// } else this
    // }
    // 
    // def find(hash: Int, key: Any) = {
      // val bit = bitpos(hash, shift)
      // 
      // if ((bitmap & bit) != 0) nodes(index(bit)).find(hash, key) else null
    // }
    // 
    // def nodeSeq = Seq.create(this, 0)
    // 
    // class Seq extends ASeq{     // TODO
      // final ISeq s
      // final int i
      // final BitmapIndexedNode node
      // 
      // 
      // Seq(ISeq s, int i, BitmapIndexedNode node){
        // this.s = s
        // this.i = i
        // this.node = node
      // }
      // 
      // Seq(IPersistentMap meta, ISeq s, int i, BitmapIndexedNode node){
        // super(meta)
        // this.s = s
        // this.i = i
        // this.node = node
      // }
      // 
      // ISeq create(BitmapIndexedNode node, int i){
        // if(i >= node.nodes.length)
				// return null
        // return new Seq(node.nodes[i].nodeSeq(), i, node)
      // }
      // 
      // public Object first(){
        // return s.first()
      // }
      // 
      // public ISeq rest(){
        // ISeq nexts = s.rest()
        // if(nexts != null)
				// return new Seq(nexts, i, node)
        // return create(node, i + 1)
      // }
      // 
      // public Seq withMeta(IPersistentMap meta){
        // return new Seq(meta, s, i, node)
      // }
    // }
    // 
    // 
  // }
  // 
  // final class LeafNode(val hash: Int, val key: Any, val value: Any) extends AMapEntry extends INode {
    // 
    // def assoc(shift: Int, hash: Int, key: Any, value: Any, addedLeaf: Box) = {
      // if (hash == this.hash) {
        // if (key == this.key) {
          // if (value == this.value) this else new LeafNode(hash, key, value)
				// } else {
          // // hash collision - same hash, different keys
          // val newLeaf = new LeafNode(hash, key, value)
          // addedLeaf.value = newLeaf
          // 
          // new HashCollisionNode(hash, this, newLeaf)
        // }
			// } else BitmapIndexedNode.create(shift, this, hash, key, value, addedLeaf)
    // }
    // 
    // public INode without(int hash, Object key){
      // if(hash == this.hash && equal(key, this.key))
			// return null
      // return this
    // }
    // 
    // public LeafNode find(int hash, Object key){
      // if(hash == this.hash && equal(key, this.key))
			// return this
      // return null
    // }
    // 
    // public ISeq nodeSeq(){
      // return RT.cons(this, null)
    // }
    // 
    // public int hash(){
      // return hash
    // }
    // 
    // public Object key(){
      // return this.key
    // }
    // 
    // public Object value(){
      // return this.value
    // }
    // 
    // public Object getKey(){
      // return this.key
    // }
    // 
    // public Object getValue(){
      // return this.value
    // }
    // 
    // public Object setValue(Object value){
      // throw new UnsupportedOperationException()
    // }
  // }
  // 
  // final class HashCollisionNode extends INode{
    // 
    // final int hash
    // final LeafNode[] leaves
    // 
    // public HashCollisionNode(int hash, LeafNode... leaves){
      // this.hash = hash
      // this.leaves = leaves
    // }
    // 
    // public INode assoc(int shift, int hash, Object key, Object value, Box addedLeaf){
      // if(hash == this.hash)
			// {
        // int idx = findIndex(hash, key)
        // if(idx != -1)
				// {
          // if(equal(leaves(idx).value, value))
					// return this
          // LeafNode[] newLeaves = leaves.clone()
          // //note  - do not set addedLeaf, since we are replacing
          // newLeaves(idx) = new LeafNode(hash, key, value)
          // return new HashCollisionNode(hash, newLeaves)
				// }
        // LeafNode[] newLeaves = new LeafNode[leaves.length + 1]
        // System.arraycopy(leaves, 0, newLeaves, 0, leaves.length)
        // addedLeaf.value = newLeaves[leaves.length] = new LeafNode(hash, key, value)
        // return new HashCollisionNode(hash, newLeaves)
			// }
      // return BitmapIndexedNode.create(shift, this, hash, key, value, addedLeaf)
    // }
    // 
    // public INode without(int hash, Object key){
      // int idx = findIndex(hash, key)
      // if(idx == -1)
			// return this
      // if(leaves.length == 2)
			// return idx == 0 ? leaves[1] : leaves[0]
      // LeafNode[] newLeaves = new LeafNode[leaves.length - 1]
      // System.arraycopy(leaves, 0, newLeaves, 0, idx)
      // System.arraycopy(leaves, idx + 1, newLeaves, idx, leaves.length - (idx + 1))
      // return new HashCollisionNode(hash, newLeaves)
    // }
    // 
    // public LeafNode find(int hash, Object key){
      // int idx = findIndex(hash, key)
      // if(idx != -1)
			// return leaves(idx)
      // return null
    // }
    // 
    // public ISeq nodeSeq(){
      // return ArraySeq.create((Object[]) leaves)
    // }
    // 
    // int findIndex(int hash, Object key){
      // for(int i = 0 i < leaves.length i++)
			// {
        // if(leaves[i].find(hash, key) != null)
				// return i
			// }
      // return -1
    // }
    // 
    // public int hash(){
      // return hash
    // }
  // }
// }
// 
// object EmptyHashMap extends HashMap[Nothing, Nothing](0, new EmptyNode())
