import { CancellationTokenSource, Disposable } from 'vscode-languageserver-protocol'
import { TreeItemCollapsibleState } from '../../tree'
import commandsManager from '../../commands'
import BasicDataProvider, { TreeNode } from '../../tree/BasicDataProvider'
import { disposeAll } from '../../util'

let disposables: Disposable[] = []

type NodeDef = [string, NodeDef[]?]

interface CustomNode extends TreeNode {
  kind?: string
  x?: number
  y?: number
}

afterEach(async () => {
  disposeAll(disposables)
  disposables = []
})

function createNode(label: string, children?: TreeNode[], key?: string, tooltip?: string): CustomNode {
  let res: TreeNode = { label }
  if (children) res.children = children
  if (tooltip) res.tooltip = tooltip
  if (key) res.key = key
  return res
}

let defaultDef: NodeDef[] = [
  ['a', [['c'], ['d']]],
  ['b', [['e'], ['f']]],
  ['g']
]

function createLabels(data: ReadonlyArray<TreeNode>): string[] {
  let res: string[] = []
  const addLabels = (n: TreeNode, level: number) => {
    res.push(' '.repeat(level) + n.label)
    if (n.children) {
      for (let node of n.children) {
        addLabels(node, level + 1)
      }
    }
  }
  for (let item of data || []) {
    addLabels(item, 0)
  }
  return res
}

function findNode(label: string, nodes: ReadonlyArray<TreeNode>): TreeNode | undefined {
  for (let n of nodes) {
    if (n.label == label) {
      return n
    }
    let children = n.children
    if (Array.isArray(children)) {
      let find = findNode(label, children)
      if (find) return find
    }
  }
}

function createNodes(defs: NodeDef[]): TreeNode[] {
  return defs.map(o => {
    let children
    if (Array.isArray(o[1])) {
      children = createNodes(o[1])
    }
    return createNode(o[0], children)
  })
}

describe('BasicDataProvider', () => {
  describe('getChildren()', () => {
    it('should get children from root', async () => {
      let nodes = createNodes(defaultDef)
      let provider = new BasicDataProvider({
        provideData: () => {
          return nodes
        }
      })
      disposables.push(provider)
      let res = await provider.getChildren()
      expect(res.length).toBe(3)
      expect(res.map(o => o.label)).toEqual(['a', 'b', 'g'])
    })

    it('should get children from child node', async () => {
      let provider = new BasicDataProvider({
        provideData: () => {
          return createNodes(defaultDef)
        }
      })
      disposables.push(provider)
      let res = await provider.getChildren()
      let nodes = await provider.getChildren(res[0])
      expect(nodes.length).toBe(2)
      expect(nodes.map(o => o.label)).toEqual(['c', 'd'])
    })

    it('should throw when provideData throws', async () => {
      let provider = new BasicDataProvider({
        provideData: () => {
          throw new Error('my error')
        }
      })
      disposables.push(provider)
      let err
      try {
        await provider.getChildren()
      } catch (e) {
        err = e
      }
      expect(err).toBeDefined()
    })
  })

  describe('getTreeItem()', () => {
    it('should get tree item from node', async () => {
      let provider = new BasicDataProvider({
        provideData: () => {
          return createNodes(defaultDef)
        }
      })
      disposables.push(provider)
      let res = await provider.getChildren()
      let item = provider.getTreeItem(res[0])
      expect(item).toBeDefined()
      expect(item.collapsibleState).toBe(TreeItemCollapsibleState.Collapsed)
      item = provider.getTreeItem(res[2])
      expect(item.collapsibleState).toBe(TreeItemCollapsibleState.None)
    })

    it('should respect expandLevel option', async () => {
      let provider = new BasicDataProvider({
        expandLevel: 1,
        provideData: () => {
          return createNodes(defaultDef)
        }
      })
      disposables.push(provider)
      let res = await provider.getChildren()
      let item = provider.getTreeItem(res[0])
      expect(item).toBeDefined()
      expect(item.collapsibleState).toBe(TreeItemCollapsibleState.Expanded)
    })

    it('should include highlights', async () => {
      let provider = new BasicDataProvider({
        provideData: () => {
          return [createNode('a', [], undefined, 'tip')]
        }
      })
      disposables.push(provider)
      let res = await provider.getChildren()
      let item = provider.getTreeItem(res[0])
      expect(item).toBeDefined()
      expect(item.tooltip).toBe('tip')
    })

    it('should use icon from node', async () => {
      let node = createNode('a', [], undefined, 'tip')
      node.icon = {
        text: 'i',
        hlGroup: 'Function'
      }
      let provider = new BasicDataProvider({
        provideData: () => {
          return [node]
        }
      })
      disposables.push(provider)
      let res = await provider.getChildren()
      let item = provider.getTreeItem(res[0])
      expect(item).toBeDefined()
      expect(item.icon).toBeDefined()
      expect(item.icon).toEqual({
        text: 'i',
        hlGroup: 'Function'
      })
    })

    it('should resolve icon', async () => {
      let provider = new BasicDataProvider<CustomNode>({
        provideData: () => {
          let node = createNode('a', [], undefined, 'tip')
          node.kind = 'function'
          return [node]
        },
        resolveIcon: item => {
          if (item.kind === 'function') {
            return {
              text: 'f',
              hlGroup: 'Function'
            }
          }
        }
      })
      disposables.push(provider)
      let res = await provider.getChildren()
      let item = provider.getTreeItem(res[0])
      expect(item).toBeDefined()
      expect(item.icon).toEqual({
        text: 'f',
        hlGroup: 'Function'
      })
    })
  })

  describe('getParent()', () => {
    it('should get undefined when data does not exist', async () => {
      let node = createNode('a')
      let provider = new BasicDataProvider({
        provideData: () => {
          return [node]
        }
      })
      disposables.push(provider)
      let res = provider.getParent(node)
      expect(res).toBeUndefined()
    })

    it('should get parent node', async () => {
      let node = createNode('g')
      let provider = new BasicDataProvider({
        provideData: () => {
          return [
            createNode('a', [createNode('c', [node]), createNode('d')]),
            createNode('b', [createNode('e'), createNode('f')]),
            createNode('g')
          ]
        }
      })
      disposables.push(provider)
      await provider.getChildren()
      let res = provider.getParent(node)
      expect(res).toBeDefined()
      expect(res.label).toBe('c')
      // console.log(provider.labels.join('\n'))
    })
  })

  describe('resolveTreeItem()', () => {
    it('should resolve tooltip and command', async () => {
      let node = createNode('a')
      let provider = new BasicDataProvider({
        provideData: () => {
          return [node]
        },
        resolveItem: item => {
          item.tooltip = 'tip'
          item.command = {
            command: 'test command',
            title: 'test'
          }
          return item
        }
      })
      disposables.push(provider)
      await provider.getChildren()
      let source = new CancellationTokenSource()
      let item = provider.getTreeItem(node)
      let resolved = await provider.resolveTreeItem(item, node, source.token)
      expect(resolved.tooltip).toBe('tip')
      expect(resolved.command.command).toBe('test command')
    })

    it('should register command invoke click', async () => {
      let node = createNode('a')
      let called: TreeNode
      let provider = new BasicDataProvider({
        provideData: () => {
          return [node]
        },
        handleClick: item => {
          called = item
        }
      })
      disposables.push(provider)
      await provider.getChildren()
      let source = new CancellationTokenSource()
      let item = provider.getTreeItem(node)
      let resolved = await provider.resolveTreeItem(item, node, source.token)
      expect(resolved.command).toBeDefined()
      expect(resolved.command.command).toMatch('invoke')
      await commandsManager.execute(resolved.command)
      expect(called).toBeDefined()
      expect(called).toBe(node)
    })
  })

  describe('update()', () => {
    it('should add children with event', async () => {
      let defs: NodeDef[] = [
        ['a', [['b']]],
        ['b', [['f']]]
      ]
      let nodes = createNodes(defs)
      let b = nodes[0].children[0]
      let provider = new BasicDataProvider({
        provideData: () => {
          return nodes
        }
      })
      disposables.push(provider)
      await provider.getChildren()
      let called = false
      provider.onDidChangeTreeData(node => {
        expect(node).toBe(b)
        called = true
      })
      let newDefs: NodeDef[] = [
        ['a', [['b', [['c'], ['d']]]]],
        ['b', [['f']]]
      ]
      let curr = provider.update(createNodes(newDefs))
      let labels = createLabels(curr)
      expect(labels).toEqual([
        'a', ' b', '  c', '  d', 'b', ' f'
      ])
      expect(called).toBe(true)
      expect(b.children).toBeDefined()
      expect(b.children.length).toBe(2)
    })

    it('should remove children with event', async () => {
      let defs: NodeDef[] = [
        ['a', [['b', [['c'], ['d']]]]],
        ['e', [['f']]]
      ]
      let nodes = createNodes(defs)
      let b = nodes[0].children[0]
      let provider = new BasicDataProvider({
        provideData: () => {
          return nodes
        }
      })
      disposables.push(provider)
      await provider.getChildren()
      let called = false
      provider.onDidChangeTreeData(node => {
        expect(node).toBe(b)
        called = true
      })
      let newDefs: NodeDef[] = [
        ['a', [['b']]],
        ['e', [['f']]]
      ]
      let curr = provider.update(createNodes(newDefs))
      let labels = createLabels(curr)
      expect(labels).toEqual([
        'a', ' b', 'e', ' f'
      ])
      expect(called).toBe(true)
      expect(b.children).toBeUndefined()
    })

    it('should not fire event for children when parent have changed', async () => {
      let defs: NodeDef[] = [
        ['a', [['b', [['c'], ['d']]]]]
      ]
      let nodes = createNodes(defs)
      let provider = new BasicDataProvider({
        provideData: () => {
          return nodes
        }
      })
      disposables.push(provider)
      await provider.getChildren()
      let called = 0
      provider.onDidChangeTreeData(node => {
        expect(node).toBeUndefined()
        called += 1
      })
      let newDefs: NodeDef[] = [
        ['a', [['b', [['c'], ['d'], ['g']]]]],
        ['e', [['f']]]
      ]
      let curr = provider.update(createNodes(newDefs))
      expect(called).toBe(1)
      let labels = createLabels(curr)
      expect(labels).toEqual([
        'a', ' b', '  c', '  d', '  g', 'e', ' f'
      ])
    })

    it('should fire events for independent node change', async () => {
      let defs: NodeDef[] = [
        ['a', [['b', [['c']]]]],
        ['e', [['f']]]
      ]
      let nodes = createNodes(defs)
      let provider = new BasicDataProvider({
        provideData: () => {
          return nodes
        }
      })
      disposables.push(provider)
      await provider.getChildren()
      let called = []
      provider.onDidChangeTreeData(node => {
        called.push(node)
      })
      let newDefs: NodeDef[] = [
        ['a', [['b', [['c'], ['d']]]]],
        ['e', [['f', [['g']]]]]
      ]
      let curr = provider.update(createNodes(newDefs))
      expect(called.length).toBe(2)
      expect(called[0].label).toBe('b')
      expect(called[1].label).toBe('f')
      let labels = createLabels(curr)
      expect(labels).toEqual([
        'a', ' b', '  c', '  d', 'e', ' f', '  g'
      ])
    })

    it('should apply new properties', async () => {
      let defs: NodeDef[] = [
        ['a', [['b']]],
        ['e', [['f']]]
      ]
      let nodes = createNodes(defs)
      let provider = new BasicDataProvider<CustomNode>({
        provideData: () => {
          return nodes
        }
      })
      disposables.push(provider)
      await provider.getChildren()
      let newNodes = createNodes([
        ['a', [['b', [['c']]]]],
        ['e', [['f', [['g']]]]]
      ])
      let b = newNodes[0].children[0]
      Object.assign(b, { x: 1, y: 2 })
      let curr = provider.update(newNodes)
      let node = curr[0].children[0]
      expect(node).toBeDefined()
      expect(node.x).toBe(1)
      expect(node.y).toBe(2)
    })

    it('should keep references and have new data sequence', async () => {
      let defs: NodeDef[] = [
        ['a', [['b'], ['c']]],
        ['e', [['f']]],
        ['g']
      ]
      let nodes = createNodes(defs)
      let keeps = [
        findNode('a', nodes),
        findNode('b', nodes),
        findNode('c', nodes),
        findNode('e', nodes),
        findNode('f', nodes),
      ]
      let provider = new BasicDataProvider({
        provideData: () => {
          return nodes
        }
      })
      disposables.push(provider)
      await provider.getChildren()
      let newNodes = createNodes([
        ['a', [['c', [['d'], ['h']]], ['b']]],
        ['e', [['f', [['j']]], ['i']]]
      ])
      let curr = provider.update(newNodes)
      expect(curr).toBe(nodes)
      expect(keeps[0]).toBe(findNode('a', curr))
      expect(keeps[1]).toBe(findNode('b', curr))
      expect(keeps[2]).toBe(findNode('c', curr))
      expect(keeps[3]).toBe(findNode('e', curr))
      expect(keeps[4]).toBe(findNode('f', curr))
      let labels = createLabels(curr)
      expect(labels).toEqual([
        'a', ' c', '  d', '  h', ' b', 'e', ' f', '  j', ' i'
      ])
    })

    it('should use key for nodes', async () => {
      let nodes = [
        createNode('a', [], 'x'),
        createNode('a', [], 'y'),
        createNode('a', [], 'z'),
      ]
      let provider = new BasicDataProvider({
        provideData: () => {
          return nodes
        }
      })
      disposables.push(provider)
      await provider.getChildren()
      let newNodes = [
        createNode('a', [], 'x'),
        createNode('a', [], 'z'),
      ]
      let curr = provider.update(newNodes)
      expect(curr.length).toBe(2)
      expect(curr[0].key).toBe('x')
      expect(curr[1].key).toBe('z')
    })

    it('should reset data', async () => {
      let nodes = [
        createNode('a', [], 'x'),
      ]
      let provider = new BasicDataProvider({
        provideData: () => {
          return nodes
        }
      })
      disposables.push(provider)
      await provider.getChildren()
      let newNodes = [
        createNode('a', [], 'x'),
      ]
      let curr = provider.update(newNodes, true)
      expect(curr === nodes).toBe(false)
    })
  })

  describe('dispose', () => {
    it('should invoke onDispose from opts', async () => {
      let called = false
      let provider = new BasicDataProvider({
        provideData: () => {
          return []
        },
        onDispose: () => {
          called = true
        }
      })
      provider.dispose()
      expect(called).toBe(true)
    })
  })
})
