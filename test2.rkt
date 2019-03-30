#lang racket


(define tree-sum
  (lambda (exp)
    (match exp                         ; 对输入exp进行模式匹配
      [(? number? x) x]                ; exp是一个数x吗?如果是,那么返回这个数x
      [`(,e1 ,e2)                      ; exp是一个由两棵子树的构成的节点吗?
       (let ([v1 (tree-sum e1)]        ; 递归调用tree-sum自己,对左子树e1求值
             [v2 (tree-sum e2)])       ; 递归调用tree-sum自己,对右子树e2求值
         (+ v1 v2))])))

(
   define tree-sum2
    (lambda(exps)
     (
      match exps 
             ((? number? x) exps)
             (`(,e1,e2)
              (let ([v1 (tree-sum2 e1)]  [v2 (tree-sum2 e2)])
              (+ v1 v2)
              ))
      ) 
      )
   )
;;;;;;模拟计算器程序
(
define calc
 (
  lambda(exps)
   {
    match exps
     ((? number? x) x)
     (`(,op,e1,e2)
      (let ([v1 (calc e1)] [ v2 (calc e2)])
      (match op
        ['+ (+ v1 v2)]
        [`- (- v1 v2)]
        [`/ (/ v1 v2)]
        [`* (* v1 v2)]
       )
        )
      )

    }

  )
 )




;;;(((lambda (x) (lambda (y) (+ x y))) 1) 2)


;;;开始动工，
;;;我们倘若我们要写解释器的话，就要维护一个公共的空间，这个空间里面放置各种数据
;;;我们用一个链表实现

;;;给定一个空的环境
(define env0 `())

;;;实现更新函数，向某个名叫env的空间更新成员
(define ext-env
  (lambda (x v env )
    (cons `(,x . , v) env)
   )
 )
;;;实现取值函数，在某个空间里面查找一个key对应的value
(
define lookup
(lambda (k env)
 (let ((q (assq k env)))
   [cond
        ((not q) #f)
        (else (cdr q))

   ]
   )
 )
)

;;闭包的数据结构定义
(struct Closure (f env))


;;解释器的递归定义
( define interp
   (lambda(exps env)
     (match exps
       [(? symbol? x) (let ( [v (  lookup x env)])  (cond [v v] [else (error"underfined variable" x )]))]
       [(? number? x) x]
       [`(lambda(,x) ,e) (Closure exps env) ] ;;;此时的exps 是一个函数，我们啥都不干就把它放进 env环境里面
       [`(let([,x,e1]) ,e2) (let ([v1 (interp e1 env)]) (interp e2 (ext-env x v1 env)))];;递归调用interp，只是为了实现let
       [`(,e1,e2)  (let([v1 (interp e1 env)] [v2 (interp e2 env)]) (match v1 [(Closure `(lambda (,x),e) env-save) (interp e (ext-env x v2 env-save)) ] ))]
       [`(,op,e1,e2) (let ([v1 (interp e1 env)] [v2 (interp e2 env)])
                       (
                        match op
                           [`+  (+ v1 v2 )]
                           [`-  (- v1 v2 )]
                           [`*  (* v1 v2 )]
                           [`/  (/ v1 v2 )]
                         )
                       )]
       )
     )
)

;;;解释器的"用户界面",隐藏戏界，留给外部gll 接口

(define G
  (lambda (exps)
    (interp  exps env0)
   )
 )